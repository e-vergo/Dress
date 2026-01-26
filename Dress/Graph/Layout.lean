/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress.Graph.Types

/-!
# Graph Layout Algorithm

Implements a simplified Sugiyama layered layout algorithm for
rendering dependency graphs.

The algorithm:
1. Assign layers (longest path from sources)
2. Order nodes within layers (barycenter heuristic)
3. Assign coordinates
-/

namespace Dress.Graph.Layout

/-- A positioned node -/
structure LayoutNode where
  /-- The original node -/
  node : Node
  /-- Horizontal position -/
  x : Float
  /-- Vertical position -/
  y : Float
  /-- Width -/
  width : Float
  /-- Height -/
  height : Float
  deriving Repr, Inhabited

/-- A positioned edge with control points -/
structure LayoutEdge where
  /-- Source node id -/
  from_ : String
  /-- Target node id -/
  to : String
  /-- Control points for bezier curve (x, y pairs) -/
  points : Array (Float × Float)
  /-- Edge style (solid or dashed) -/
  style : EdgeStyle := .solid
  deriving Repr, Inhabited

/-- The complete laid-out graph -/
structure LayoutGraph where
  /-- Positioned nodes -/
  nodes : Array LayoutNode
  /-- Positioned edges -/
  edges : Array LayoutEdge
  /-- Total width -/
  width : Float
  /-- Total height -/
  height : Float
  deriving Repr, Inhabited

/-- Layout configuration for top-to-bottom flow -/
structure LayoutConfig where
  /-- Minimum width of a node box -/
  nodeWidth : Float := 100.0
  /-- Height of a node box -/
  nodeHeight : Float := 40.0
  /-- Vertical gap between layers (y-axis distance) -/
  layerGap : Float := 100.0
  /-- Horizontal gap between nodes in the same layer (x-axis distance) -/
  nodeGap : Float := 40.0
  /-- Padding around the graph -/
  padding : Float := 20.0
  /-- Pixels per character for dynamic node width -/
  charWidth : Float := 8.0
  /-- Number of barycenter iterations for crossing reduction -/
  barycenterIterations : Nat := 4
  deriving Repr, Inhabited

/-- Compute node width based on label length -/
def computeNodeWidth (config : LayoutConfig) (label : String) : Float :=
  let labelWidth := label.length.toFloat * config.charWidth
  if labelWidth > config.nodeWidth then labelWidth else config.nodeWidth

/-! ## Edge Routing: Boundary Intersection Helpers -/

/-- Line-ellipse intersection. Returns point on ellipse boundary.
    Given ellipse center (cx, cy), radii (rx, ry), and target point (px, py),
    finds where the line from center to target intersects the ellipse. -/
def intersectLineEllipse (cx cy rx ry : Float) (px py : Float) : Float × Float :=
  -- Direction from center to point
  let dx := px - cx
  let dy := py - cy
  -- Parametric: find t where (cx + t*dx, cy + t*dy) is on ellipse
  -- (t*dx/rx)² + (t*dy/ry)² = 1
  -- t² * (dx²/rx² + dy²/ry²) = 1
  let denom := (dx * dx) / (rx * rx) + (dy * dy) / (ry * ry)
  if denom <= 0 then (cx, cy) else
    let t := 1.0 / Float.sqrt denom
    (cx + t * dx, cy + t * dy)

/-- Line-rectangle intersection. Returns point on rectangle boundary.
    Given rect at (x, y) with dimensions (w, h), and target point (px, py),
    finds where the line from center to target intersects the rectangle. -/
def intersectLineRect (x y w h : Float) (px py : Float) : Float × Float :=
  let cx := x + w / 2
  let cy := y + h / 2
  let dx := px - cx
  let dy := py - cy
  if dx == 0 && dy == 0 then (cx, cy) else
    -- Find intersection with each edge, take the closest (smallest positive t)
    let tRight := if dx > 0 then (w / 2) / dx else 1e10
    let tLeft := if dx < 0 then (-w / 2) / dx else 1e10
    let tBottom := if dy > 0 then (h / 2) / dy else 1e10
    let tTop := if dy < 0 then (-h / 2) / dy else 1e10
    let t := min (min tRight tLeft) (min tBottom tTop)
    (cx + t * dx, cy + t * dy)

/-- Clip edge endpoint to node boundary based on shape.
    Given a LayoutNode and a target point, returns the point on the node's
    boundary along the line from node center to target. -/
def clipToNodeBoundary (node : LayoutNode) (targetX targetY : Float) : Float × Float :=
  let cx := node.x + node.width / 2
  let cy := node.y + node.height / 2
  match node.node.shape with
  | .ellipse =>
    let rx := node.width / 2
    let ry := node.height / 2
    intersectLineEllipse cx cy rx ry targetX targetY
  | .box =>
    intersectLineRect node.x node.y node.width node.height targetX targetY

/-! ## Visibility Graph for Edge Routing -/

/-- A point in 2D space for edge routing -/
structure Point where
  x : Float
  y : Float
  deriving Repr, Inhabited, BEq

/-- An obstacle (node bounding box with margin) for visibility graph -/
structure Obstacle where
  x : Float  -- Top-left X
  y : Float  -- Top-left Y
  w : Float  -- Width
  h : Float  -- Height
  shape : NodeShape
  deriving Repr, Inhabited

/-- Get visibility vertices for a rectangular obstacle (4 corners expanded by margin) -/
def rectObstacleVertices (obs : Obstacle) (margin : Float) : Array Point :=
  let x1 := obs.x - margin
  let y1 := obs.y - margin
  let x2 := obs.x + obs.w + margin
  let y2 := obs.y + obs.h + margin
  #[⟨x1, y1⟩, ⟨x2, y1⟩, ⟨x1, y2⟩, ⟨x2, y2⟩]

/-- Get visibility vertices for an elliptical obstacle (8 octant points) -/
def ellipseObstacleVertices (obs : Obstacle) (margin : Float) : Array Point :=
  let cx := obs.x + obs.w / 2
  let cy := obs.y + obs.h / 2
  let rx := obs.w / 2 + margin
  let ry := obs.h / 2 + margin
  let sqrt2_2 : Float := 0.7071067811865476  -- √2/2
  #[
    ⟨cx + rx, cy⟩,
    ⟨cx + rx * sqrt2_2, cy - ry * sqrt2_2⟩,
    ⟨cx, cy - ry⟩,
    ⟨cx - rx * sqrt2_2, cy - ry * sqrt2_2⟩,
    ⟨cx - rx, cy⟩,
    ⟨cx - rx * sqrt2_2, cy + ry * sqrt2_2⟩,
    ⟨cx, cy + ry⟩,
    ⟨cx + rx * sqrt2_2, cy + ry * sqrt2_2⟩
  ]

/-- Check if line segment intersects rectangle using Liang-Barsky algorithm.
    Returns true if segment from p1 to p2 intersects the rectangle interior. -/
def segmentIntersectsRect (p1 p2 : Point) (obs : Obstacle) : Bool :=
  let dx := p2.x - p1.x
  let dy := p2.y - p1.y
  let xmin := obs.x
  let xmax := obs.x + obs.w
  let ymin := obs.y
  let ymax := obs.y + obs.h

  -- Liang-Barsky algorithm: clip segment to rectangle
  -- p values represent direction toward each boundary
  -- q values represent signed distance from p1 to each boundary
  let p := #[-dx, dx, -dy, dy]
  let q := #[p1.x - xmin, xmax - p1.x, p1.y - ymin, ymax - p1.y]

  -- Find valid t range [t0, t1] for the segment
  let (t0, t1, valid) := Id.run do
    let mut t0 : Float := 0.0
    let mut t1 : Float := 1.0
    for i in [0:4] do
      let pi := p[i]!
      let qi := q[i]!
      if pi == 0 then
        -- Line parallel to this boundary
        if qi < 0 then
          return (t0, t1, false)  -- Outside and parallel - no intersection
      else
        let t := qi / pi
        if pi < 0 then
          -- Entering: update t0 (start of valid range)
          t0 := max t0 t
        else
          -- Leaving: update t1 (end of valid range)
          t1 := min t1 t
      if t0 > t1 then
        return (t0, t1, false)  -- Ranges don't overlap - no intersection
    return (t0, t1, true)

  valid && t0 <= t1

/-- Check if line segment intersects ellipse (parametric approach).
    Returns true if segment from p1 to p2 intersects the ellipse interior. -/
def segmentIntersectsEllipse (p1 p2 : Point) (obs : Obstacle) : Bool :=
  -- Transform to unit circle centered at origin
  let cx := obs.x + obs.w / 2
  let cy := obs.y + obs.h / 2
  let rx := obs.w / 2
  let ry := obs.h / 2

  -- Handle degenerate ellipse
  if rx <= 0 || ry <= 0 then false
  else
    -- Normalize points relative to ellipse center and radii
    let x1 := (p1.x - cx) / rx
    let y1 := (p1.y - cy) / ry
    let x2 := (p2.x - cx) / rx
    let y2 := (p2.y - cy) / ry

    -- Now check intersection with unit circle
    -- Line: P = P1 + t*(P2-P1), t in [0,1]
    -- Circle: x² + y² = 1
    let dx := x2 - x1
    let dy := y2 - y1

    -- Quadratic: At² + Bt + C = 0
    let a := dx*dx + dy*dy
    let b := 2*(x1*dx + y1*dy)
    let c := x1*x1 + y1*y1 - 1

    -- Handle degenerate case (point, not a line segment)
    if a < 1e-10 then c <= 0  -- Point inside circle
    else
      let discriminant := b*b - 4*a*c
      if discriminant < 0 then false
      else
        let sqrtD := Float.sqrt discriminant
        let t1 := (-b - sqrtD) / (2*a)
        let t2 := (-b + sqrtD) / (2*a)

        -- Check if any intersection is within segment [0, 1]
        -- Also check if segment is entirely inside (t1 < 0 && t2 > 1)
        (0 <= t1 && t1 <= 1) || (0 <= t2 && t2 <= 1) || (t1 < 0 && t2 > 1)

/-- Check if line segment intersects any obstacle (excluding specified indices).
    excludeIndices allows skipping the source and target node obstacles. -/
def segmentIntersectsObstacles (p1 p2 : Point) (obstacles : Array Obstacle)
    (excludeIndices : Array Nat) : Bool := Id.run do
  for i in [0:obstacles.size] do
    if !excludeIndices.contains i then
      let obs := obstacles[i]!
      let intersects := match obs.shape with
        | .box => segmentIntersectsRect p1 p2 obs
        | .ellipse => segmentIntersectsEllipse p1 p2 obs
      if intersects then
        return true
  return false

/-- Collect all visibility vertices from obstacles plus source and target -/
def collectVisibilityVertices (source target : Point) (obstacles : Array Obstacle)
    (margin : Float := 5.0) : Array Point := Id.run do
  let mut vertices : Array Point := #[source]
  for obs in obstacles do
    let obsVerts := match obs.shape with
      | .box => rectObstacleVertices obs margin
      | .ellipse => ellipseObstacleVertices obs margin
    vertices := vertices ++ obsVerts
  vertices := vertices.push target
  return vertices

/-- Build visibility graph edges (pairs of vertex indices that can see each other).
    Returns an array of (i, j) pairs where vertices[i] and vertices[j] have
    unobstructed line of sight. -/
def buildVisibilityGraph (vertices : Array Point) (obstacles : Array Obstacle)
    (excludeIndices : Array Nat := #[]) : Array (Nat × Nat) := Id.run do
  let mut edges : Array (Nat × Nat) := #[]
  for i in [0:vertices.size] do
    for j in [i+1:vertices.size] do
      let p1 := vertices[i]!
      let p2 := vertices[j]!
      if !segmentIntersectsObstacles p1 p2 obstacles excludeIndices then
        edges := edges.push (i, j)
        edges := edges.push (j, i)  -- Undirected graph
  return edges

/-! ### Dijkstra's Shortest Path -/

/-- Euclidean distance between two points -/
def Point.dist (p1 p2 : Point) : Float :=
  let dx := p2.x - p1.x
  let dy := p2.y - p1.y
  Float.sqrt (dx * dx + dy * dy)

/-- Find shortest path using Dijkstra's algorithm.
    Returns array of vertex indices from source to target.
    Uses O(V²) implementation which is sufficient for small visibility graphs. -/
def dijkstraShortestPath (vertices : Array Point) (edges : Array (Nat × Nat))
    (sourceIdx targetIdx : Nat) : Array Nat := Id.run do
  let n := vertices.size
  if n == 0 || sourceIdx >= n || targetIdx >= n then return #[]

  -- Handle trivial case: source == target
  if sourceIdx == targetIdx then return #[sourceIdx]

  -- Build adjacency list with distances
  let mut adj : Array (Array (Nat × Float)) := List.replicate n #[] |>.toArray
  for (i, j) in edges do
    if i < n && j < n then
      let d := Point.dist vertices[i]! vertices[j]!
      adj := adj.modify i (·.push (j, d))

  -- Distance array (infinity = very large number)
  let infinity : Float := 1e18
  let mut dist : Array Float := List.replicate n infinity |>.toArray
  dist := dist.set! sourceIdx 0.0

  -- Previous vertex for path reconstruction
  let mut prev : Array (Option Nat) := List.replicate n none |>.toArray

  -- Visited array
  let mut visited : Array Bool := List.replicate n false |>.toArray

  -- Track if we found the target
  let mut foundTarget := false

  -- Simple O(V²) Dijkstra (sufficient for small graphs)
  for _ in [0:n] do
    if foundTarget then
      pure ()  -- Skip remaining iterations
    else
      -- Find unvisited vertex with minimum distance
      let mut minDist := infinity
      let mut u : Option Nat := none
      for i in [0:n] do
        if !visited[i]! && dist[i]! < minDist then
          minDist := dist[i]!
          u := some i

      match u with
      | none => foundTarget := true  -- No reachable unvisited vertices, stop
      | some uIdx =>
        if uIdx == targetIdx then
          foundTarget := true  -- Found target, stop
        else
          visited := visited.set! uIdx true

          -- Relax edges from u
          for (v, weight) in adj[uIdx]! do
            let alt := dist[uIdx]! + weight
            if alt < dist[v]! then
              dist := dist.set! v alt
              prev := prev.set! v (some uIdx)

  -- Reconstruct path
  if dist[targetIdx]! >= infinity then return #[]  -- No path found

  -- Build path backwards from target to source
  let mut path : Array Nat := #[targetIdx]
  let mut current := targetIdx
  let mut pathLength := 0
  let maxPathLength := n  -- Prevent infinite loops

  while pathLength < maxPathLength do
    pathLength := pathLength + 1
    match prev[current]! with
    | none =>
      -- Reached a node with no predecessor
      if current == sourceIdx then
        -- Successfully reached source, reverse and return
        return path.reverse
      else
        -- Broken path, shouldn't happen if dist[targetIdx] < infinity
        return #[]
    | some p =>
      path := path.push p
      current := p

  -- If we exit the loop without returning, path reconstruction failed
  return #[]

/-- Get the actual Point coordinates from a path of indices -/
def getPathPoints (vertices : Array Point) (path : Array Nat) : Array Point :=
  path.filterMap fun i => vertices[i]?

/-! ### Bezier Curve Fitting -/

/-- Convert 4 Catmull-Rom points to cubic Bezier control points.
    Given points P0, P1, P2, P3, the curve goes from P1 to P2.
    Returns (P1, CP1, CP2, P2) where CP1 and CP2 are Bezier control points. -/
def catmullRomToBezier (p0 p1 p2 p3 : Point) (tension : Float := 0.5)
    : Point × Point × Point × Point :=
  -- Catmull-Rom tangent at P1: (P2 - P0) / 2
  -- Catmull-Rom tangent at P2: (P3 - P1) / 2
  -- Bezier control point 1: P1 + tangent1 / 3
  -- Bezier control point 2: P2 - tangent2 / 3
  let t := tension
  let cp1 : Point := {
    x := p1.x + (p2.x - p0.x) * t / 3
    y := p1.y + (p2.y - p0.y) * t / 3
  }
  let cp2 : Point := {
    x := p2.x - (p3.x - p1.x) * t / 3
    y := p2.y - (p3.y - p1.y) * t / 3
  }
  (p1, cp1, cp2, p2)

/-- Convert a polyline (array of points) to Bezier control points for smooth curve.
    Returns array of points suitable for SVG cubic Bezier path:
    [start, cp1, cp2, p1, cp3, cp4, p2, ...]

    For 2 points: straight line (no control points needed, just endpoints)
    For 3+ points: Catmull-Rom interpolation with phantom endpoints -/
def polylineToBezier (waypoints : Array Point) : Array Point := Id.run do
  if waypoints.size < 2 then return waypoints
  if waypoints.size == 2 then return waypoints  -- Just a straight line

  let mut result : Array Point := #[]
  let n := waypoints.size

  -- Add the starting point
  result := result.push waypoints[0]!

  -- For each segment, we need 4 points for Catmull-Rom
  -- For edge segments, we create phantom points by reflection
  for i in [0:n-1] do
    -- Get P0, P1, P2, P3 for Catmull-Rom
    let p0 := if i == 0 then
      -- Phantom point: reflect P1 about P0
      let p0' := waypoints[0]!
      let p1' := waypoints[1]!
      { x := 2 * p0'.x - p1'.x, y := 2 * p0'.y - p1'.y : Point }
    else
      waypoints[i-1]!

    let p1 := waypoints[i]!
    let p2 := waypoints[i+1]!

    let p3 := if i + 2 >= n then
      -- Phantom point: reflect P2 about P3 (which is the last point)
      let pLast := waypoints[n-1]!
      let pPrev := waypoints[n-2]!
      { x := 2 * pLast.x - pPrev.x, y := 2 * pLast.y - pPrev.y : Point }
    else
      waypoints[i+2]!

    let (_, cp1, cp2, endPt) := catmullRomToBezier p0 p1 p2 p3

    -- Add control points and endpoint for this cubic Bezier segment
    result := result.push cp1
    result := result.push cp2
    result := result.push endPt

  return result

/-- Convert Bezier control points to SVG path data string.
    Input: [start, cp1, cp2, end1, cp3, cp4, end2, ...]
    Output: "M x0 y0 C cp1x cp1y cp2x cp2y x1 y1 ..." -/
def bezierToSvgPath (points : Array Point) : String := Id.run do
  if points.size < 2 then return ""

  let mut path := s!"M {points[0]!.x} {points[0]!.y}"

  if points.size == 2 then
    -- Straight line
    path := path ++ s!" L {points[1]!.x} {points[1]!.y}"
    return path

  -- Process cubic Bezier segments (groups of 3 after start: cp1, cp2, end)
  let mut i := 1
  while i + 2 < points.size do
    let cp1 := points[i]!
    let cp2 := points[i+1]!
    let endPt := points[i+2]!
    path := path ++ s!" C {cp1.x} {cp1.y} {cp2.x} {cp2.y} {endPt.x} {endPt.y}"
    i := i + 3

  return path

/-- Simple corner smoothing: for each interior waypoint, create a smooth corner.
    Returns control points for quadratic Bezier curves mixed with line segments. -/
def smoothCorners (waypoints : Array Point) (cornerRadius : Float := 10.0)
    : Array Point := Id.run do
  if waypoints.size < 3 then return waypoints

  let mut result : Array Point := #[waypoints[0]!]

  for i in [1:waypoints.size - 1] do
    let prev := waypoints[i-1]!
    let curr := waypoints[i]!
    let next := waypoints[i+1]!

    -- Vector from current to prev
    let toPrev : Point := { x := prev.x - curr.x, y := prev.y - curr.y }
    let lenPrev := Float.sqrt (toPrev.x * toPrev.x + toPrev.y * toPrev.y)

    -- Vector from current to next
    let toNext : Point := { x := next.x - curr.x, y := next.y - curr.y }
    let lenNext := Float.sqrt (toNext.x * toNext.x + toNext.y * toNext.y)

    -- Don't smooth if segments are too short
    let r := min cornerRadius (min (lenPrev / 2) (lenNext / 2))

    if r > 0.1 && lenPrev > 0.1 && lenNext > 0.1 then
      -- Points where the curve starts and ends
      let startPt : Point := {
        x := curr.x + toPrev.x / lenPrev * r
        y := curr.y + toPrev.y / lenPrev * r
      }
      let endPt : Point := {
        x := curr.x + toNext.x / lenNext * r
        y := curr.y + toNext.y / lenNext * r
      }
      -- The corner point becomes the control point for quadratic Bezier
      result := result.push startPt
      result := result.push curr  -- control point
      result := result.push endPt
    else
      result := result.push curr

  result := result.push waypoints[waypoints.size - 1]!
  return result

/-- Convert Point to Float tuple -/
def Point.toTuple (p : Point) : Float × Float := (p.x, p.y)

/-- Convert Float tuple to Point -/
def Point.fromTuple (t : Float × Float) : Point := ⟨t.1, t.2⟩

/-- Convert smoothed corners output to SVG path data string.
    Handles mixed line segments and quadratic Bezier curves.
    Input pattern from smoothCorners: [start, ..., startPt, controlPt, endPt, ..., end]
    where triplets (startPt, controlPt, endPt) are quadratic curves. -/
def smoothCornersToSvgPath (points : Array Point) : String := Id.run do
  if points.size < 2 then return ""

  let mut path := s!"M {points[0]!.x} {points[0]!.y}"
  let mut i := 1

  while i < points.size do
    -- Check if we have a quadratic curve triplet
    -- This is detected by checking if the next two points could form a curve
    -- For now, we treat every 3 points after start as potential curve
    if i + 2 < points.size then
      -- Assume this could be a quadratic Bezier (line to start, curve through control to end)
      let p1 := points[i]!
      let cp := points[i+1]!
      let p2 := points[i+2]!

      -- Line to curve start, then quadratic curve
      path := path ++ s!" L {p1.x} {p1.y}"
      path := path ++ s!" Q {cp.x} {cp.y} {p2.x} {p2.y}"
      i := i + 3
    else
      -- Just a line segment to the next point
      let p := points[i]!
      path := path ++ s!" L {p.x} {p.y}"
      i := i + 1

  return path

/-- Layout state -/
structure LayoutState where
  /-- Node to layer assignment -/
  layers : Std.HashMap String Nat := {}
  /-- Nodes in each layer -/
  layerNodes : Array (Array String) := #[]
  /-- Node positions -/
  positions : Std.HashMap String (Float × Float) := {}
  /-- Dynamic node widths based on labels -/
  nodeWidths : Std.HashMap String Float := {}
  deriving Inhabited

/-- Layout monad -/
abbrev LayoutM := StateM LayoutState

namespace Algorithm

/-! ## Crossing Reduction Helpers -/

/-- Count edge crossings between two adjacent layers.
    An edge (u1, v1) crosses edge (u2, v2) if u1 < u2 and v1 > v2 (or vice versa).
    This counts inversions between edge endpoints. -/
def countCrossings (g : Graph) (layerNodes : Array (Array String))
    (layer1Idx layer2Idx : Nat) : Nat :=
  if layer1Idx < layerNodes.size then
    if layer2Idx < layerNodes.size then
      let layer1 := layerNodes[layer1Idx]!
      let layer2 := layerNodes[layer2Idx]!
      -- Get edges between these layers
      let edges := g.edges.filter fun e =>
        layer1.contains e.from_ && layer2.contains e.to
      -- For each pair of edges, check for crossing using fold
      Id.run do
        let mut crossings := 0
        for i in [0:edges.size] do
          for j in [i+1:edges.size] do
            let e1 := edges[i]!
            let e2 := edges[j]!
            let u1 := layer1.findIdx? (· == e1.from_) |>.getD 0
            let u2 := layer1.findIdx? (· == e2.from_) |>.getD 0
            let v1 := layer2.findIdx? (· == e1.to) |>.getD 0
            let v2 := layer2.findIdx? (· == e2.to) |>.getD 0
            -- Crossing if the order is inverted
            if (u1 < u2 && v1 > v2) || (u1 > u2 && v1 < v2) then
              crossings := crossings + 1
        return crossings
    else 0
  else 0

/-- Count total edge crossings in the entire graph -/
def countTotalCrossings (g : Graph) (layerNodes : Array (Array String)) : Nat := Id.run do
  let mut total := 0
  for i in [0:layerNodes.size - 1] do
    total := total + countCrossings g layerNodes i (i + 1)
  return total

/-- Swap two elements in an array -/
def swapArray (arr : Array α) (i j : Nat) : Array α :=
  if hi : i < arr.size then
    if hj : j < arr.size then
      let vi := arr[i]
      let vj := arr[j]
      arr.set! i vj |>.set! j vi
    else arr
  else arr

/-- Try swapping two adjacent nodes in a layer, return new layer if it reduces crossings -/
def trySwapAdjacent (g : Graph) (layerNodes : Array (Array String))
    (layerIdx : Nat) (i j : Nat) : Option (Array String) :=
  if layerIdx < layerNodes.size then
    let layer := layerNodes[layerIdx]!
    if i < layer.size then
      if j < layer.size then
        -- Compute crossings before swap
        let crossingsBefore := if layerIdx > 0 then
            countCrossings g layerNodes (layerIdx - 1) layerIdx
          else 0
        let crossingsAfter := if layerIdx + 1 < layerNodes.size then
            countCrossings g layerNodes layerIdx (layerIdx + 1)
          else 0
        let totalBefore := crossingsBefore + crossingsAfter

        -- Swap nodes
        let swapped := swapArray layer i j

        -- Update layerNodes temporarily for crossing count
        let newLayerNodes := layerNodes.set! layerIdx swapped

        -- Compute crossings after swap
        let crossingsBeforeNew := if layerIdx > 0 then
            countCrossings g newLayerNodes (layerIdx - 1) layerIdx
          else 0
        let crossingsAfterNew := if layerIdx + 1 < newLayerNodes.size then
            countCrossings g newLayerNodes layerIdx (layerIdx + 1)
          else 0
        let totalAfter := crossingsBeforeNew + crossingsAfterNew

        -- Keep swap if it reduces crossings
        if totalAfter < totalBefore then
          some swapped
        else
          none
      else none
    else none
  else none

/-- Transpose heuristic: repeatedly try swapping adjacent nodes to reduce crossings -/
def transposePass (g : Graph) (layerNodes : Array (Array String)) : Array (Array String) := Id.run do
  let mut result := layerNodes
  let mut improved := true
  let mut iterations := 0
  let maxIterations := 10  -- Prevent infinite loops

  while improved && iterations < maxIterations do
    improved := false
    iterations := iterations + 1
    for layerIdx in [0:result.size] do
      let layer := result[layerIdx]!
      for i in [0:layer.size - 1] do
        match trySwapAdjacent g result layerIdx i (i + 1) with
        | some newLayer =>
          result := result.set! layerIdx newLayer
          improved := true
        | none => pure ()

  return result

/-! ## Median Calculation -/

/-- Calculate median of a list of floats -/
def median (values : Array Float) : Float :=
  if values.isEmpty then 0.0
  else
    let sorted := values.qsort (· < ·)
    let mid := sorted.size / 2
    if sorted.size % 2 == 0 then
      -- Even number: average of two middle values
      (sorted[mid - 1]! + sorted[mid]!) / 2.0
    else
      sorted[mid]!

/-- Get the median position of neighbors in a reference layer.
    For DOT-style layout, we use median instead of mean for robustness. -/
def medianNeighborPosition (g : Graph) (layerNodes : Array (Array String))
    (nodeId : String) (layerIdx refLayerIdx : Nat) (forward : Bool) : Option Float :=
  if h1 : layerIdx < layerNodes.size then
    if h2 : refLayerIdx < layerNodes.size then
      let refLayer := layerNodes[refLayerIdx]

      -- Get edges connecting to reference layer
      let neighborEdges := if forward then
        g.edges.filter (·.to == nodeId)  -- Incoming edges
      else
        g.edges.filter (·.from_ == nodeId)  -- Outgoing edges

      let refPositions := neighborEdges.filterMap fun e =>
        let neighborId := if forward then e.from_ else e.to
        refLayer.findIdx? (· == neighborId) |>.map (·.toFloat)

      if refPositions.isEmpty then
        none
      else
        some (median refPositions)
    else none
  else none

/-- Order a single layer by median of neighbors in the reference layer -/
def orderLayerByMedian (g : Graph) (layerNodes : Array (Array String))
    (layerIdx : Nat) (refLayerIdx : Nat) (forward : Bool) : Array String :=
  if h : layerIdx < layerNodes.size then
    let layer := layerNodes[layerIdx]

    -- Calculate median position for each node
    let medians := layer.map fun nodeId =>
      let med := medianNeighborPosition g layerNodes nodeId layerIdx refLayerIdx forward
      match med with
      | some m => (nodeId, m)
      | none =>
        -- Keep original position if no neighbors
        let origIdx := layer.findIdx? (· == nodeId) |>.getD 0
        (nodeId, origIdx.toFloat)

    -- Sort by median
    medians.qsort (·.2 < ·.2) |>.map (·.1)
  else #[]

/-- Assign layers using longest path from sources -/
def assignLayers (g : Graph) : LayoutM Unit := do
  -- Find source nodes (no incoming edges)
  let sources := g.nodes.filter fun n =>
    g.edges.all fun e => e.to != n.id

  -- BFS to assign layers
  let mut queue := sources.map (·.id)
  let mut visited : Std.HashSet String := {}

  for src in sources.map (·.id) do
    modify fun s => { s with layers := s.layers.insert src 0 }
    visited := visited.insert src

  while !queue.isEmpty do
    let current := queue[0]!
    queue := queue.toSubarray.drop 1 |>.toArray

    let currentLayer := (← get).layers.get? current |>.getD 0

    -- Process outgoing edges
    for edge in g.edges do
      if edge.from_ == current then
        let targetLayer := currentLayer + 1
        let s ← get
        let existingLayer := s.layers.get? edge.to |>.getD 0
        if targetLayer > existingLayer then
          set { s with layers := s.layers.insert edge.to targetLayer }

        if !visited.contains edge.to then
          visited := visited.insert edge.to
          queue := queue.push edge.to

  -- Build layer arrays
  let s ← get
  let maxLayer := s.layers.fold (init := 0) fun acc _ layer => max acc layer

  let mut layerNodes : Array (Array String) := #[]
  for _ in [0:maxLayer + 1] do
    layerNodes := layerNodes.push #[]
  for (nodeId, layer) in s.layers.toArray do
    layerNodes := layerNodes.modify layer (·.push nodeId)

  set { s with layerNodes }

/-- Order a single layer by barycenter of neighbors in the reference layer
    forward=true means reference is previous layer (i-1), false means next layer (i+1) -/
def orderLayerByNeighbors (g : Graph) (layerNodes : Array (Array String))
    (layerIdx : Nat) (refLayerIdx : Nat) (forward : Bool) : Array String :=
  let layer := layerNodes[layerIdx]!
  let refLayer := layerNodes[refLayerIdx]!

  -- Calculate barycenter for each node based on neighbor positions
  let barycenters := layer.map fun nodeId =>
    let neighborEdges := if forward then
      -- Forward pass: look at incoming edges (neighbors in previous layer)
      g.edges.filter (·.to == nodeId)
    else
      -- Backward pass: look at outgoing edges (neighbors in next layer)
      g.edges.filter (·.from_ == nodeId)

    let refPositions := neighborEdges.filterMap fun e =>
      let neighborId := if forward then e.from_ else e.to
      refLayer.findIdx? (· == neighborId) |>.map (·.toFloat)

    let avg := if refPositions.isEmpty then
      -- Keep original position if no neighbors in reference layer
      layer.findIdx? (· == nodeId) |>.map (·.toFloat) |>.getD 0.0
    else
      refPositions.foldl (· + ·) 0.0 / refPositions.size.toFloat
    (nodeId, avg)

  -- Sort by barycenter
  barycenters.qsort (·.2 < ·.2) |>.map (·.1)

/-- Order nodes within layers using median heuristic with transpose optimization.
    DOT algorithm approach:
    1. Multiple iterations of forward/backward median ordering
    2. Transpose heuristic after each pass to further reduce crossings -/
def orderLayers (g : Graph) (iterations : Nat) : LayoutM Unit := do
  let s ← get
  let mut layerNodes := s.layerNodes

  -- Run multiple iterations alternating forward and backward passes
  for _iter in [0:iterations] do
    -- Forward pass: order each layer by median of previous layer neighbors
    for i in [1:layerNodes.size] do
      let sorted := orderLayerByMedian g layerNodes i (i-1) true
      layerNodes := layerNodes.set! i sorted

    -- Apply transpose heuristic after forward pass
    layerNodes := transposePass g layerNodes

    -- Backward pass: order each layer by median of next layer neighbors
    if layerNodes.size > 1 then
      for i in List.range (layerNodes.size - 1) |>.reverse do
        let sorted := orderLayerByMedian g layerNodes i (i+1) false
        layerNodes := layerNodes.set! i sorted

    -- Apply transpose heuristic after backward pass
    layerNodes := transposePass g layerNodes

  set { s with layerNodes }

/-! ## Median-Based Coordinate Assignment (for top-to-bottom layout) -/

/-- Get all neighbor X positions for a node (from all connected layers) -/
def getNeighborXPositions (g : Graph) (positions : Std.HashMap String (Float × Float))
    (nodeId : String) : Array Float := Id.run do
  let mut xPositions : Array Float := #[]

  -- Get neighbors from incoming edges
  for edge in g.edges do
    if edge.to == nodeId then
      match positions.get? edge.from_ with
      | some (x, _) => xPositions := xPositions.push x
      | none => pure ()
    if edge.from_ == nodeId then
      match positions.get? edge.to with
      | some (x, _) => xPositions := xPositions.push x
      | none => pure ()

  return xPositions

/-- Calculate ideal X position based on median of all neighbor X positions -/
def idealXPosition (g : Graph) (positions : Std.HashMap String (Float × Float))
    (nodeId : String) : Option Float :=
  let neighborXs := getNeighborXPositions g positions nodeId
  if neighborXs.isEmpty then
    none
  else
    -- Use median for robustness
    some (median neighborXs)

/-- Resolve overlaps within a layer by pushing nodes apart horizontally -/
def resolveOverlaps (layer : Array String) (positions : Std.HashMap String (Float × Float))
    (nodeWidths : Std.HashMap String Float) (config : LayoutConfig) : Std.HashMap String (Float × Float) := Id.run do
  let mut result := positions

  -- Sort nodes by X position
  let nodesWithX := layer.filterMap fun nodeId =>
    positions.get? nodeId |>.map fun (x, y) => (nodeId, x, y)

  let sorted := nodesWithX.qsort (fun a b => a.2.1 < b.2.1)

  -- Push overlapping nodes apart, tracking the actual X positions as we update
  let mut lastX : Float := -1000.0  -- Start with a very low value
  let mut lastWidth : Float := 0.0

  for i in [0:sorted.size] do
    let (currId, currX, currY) := sorted[i]!
    let currWidth := nodeWidths.get? currId |>.getD config.nodeWidth

    -- Get the actual current X (which may have been updated)
    let actualX := match result.get? currId with
      | some (x, _) => x
      | none => currX

    if i == 0 then
      lastX := actualX
      lastWidth := currWidth
    else
      let minSeparation := lastWidth + config.nodeGap
      let gap := actualX - lastX
      if gap < minSeparation then
        -- Push current node right
        let newX := lastX + minSeparation
        result := result.insert currId (newX, currY)
        lastX := newX
      else
        lastX := actualX
      lastWidth := currWidth

  return result

/-- Single pass of position refinement: move nodes toward neighbor medians -/
def refinePositionsPass (g : Graph) (layerNodes : Array (Array String))
    (positions : Std.HashMap String (Float × Float))
    (nodeWidths : Std.HashMap String Float) (config : LayoutConfig)
    (forward : Bool) : Std.HashMap String (Float × Float) := Id.run do
  let mut result := positions

  let layerIndices := if forward then
    List.range layerNodes.size
  else
    List.range layerNodes.size |>.reverse

  for layerIdx in layerIndices do
    let layer := layerNodes[layerIdx]!

    -- Calculate ideal positions for this layer
    for nodeId in layer do
      match (result.get? nodeId, idealXPosition g result nodeId) with
      | (some (_currentX, y), some idealX) =>
        -- Move toward ideal position (but keep Y fixed)
        result := result.insert nodeId (idealX, y)
      | _ => pure ()

    -- Resolve overlaps in this layer
    result := resolveOverlaps layer result nodeWidths config

  return result

/-- Iteratively refine positions to achieve organic layout -/
def refinePositions (g : Graph) (config : LayoutConfig) (refinementIterations : Nat) : LayoutM Unit := do
  let s ← get
  let mut positions := s.positions

  for _iter in [0:refinementIterations] do
    -- Forward pass
    positions := refinePositionsPass g s.layerNodes positions s.nodeWidths config true
    -- Backward pass
    positions := refinePositionsPass g s.layerNodes positions s.nodeWidths config false

  set { s with positions }

/-- Assign initial coordinates to nodes (grid-based starting point)
    Top-to-bottom layout:
    - Layer index determines y position (sources at top, sinks at bottom)
    - Node index within layer determines initial x position -/
def assignInitialCoordinates (g : Graph) (config : LayoutConfig) : LayoutM Unit := do
  let s ← get
  let mut positions : Std.HashMap String (Float × Float) := {}
  let mut nodeWidths : Std.HashMap String Float := {}

  -- First, compute dynamic widths for all nodes
  for node in g.nodes do
    let width := computeNodeWidth config node.label
    nodeWidths := nodeWidths.insert node.id width

  -- Find the maximum layer width (for horizontal centering)
  -- Calculate total width needed for each layer
  let layerTotalWidths := s.layerNodes.map fun layer =>
    let widthSum := layer.foldl (fun acc nodeId =>
      let w := nodeWidths.get? nodeId |>.getD config.nodeWidth
      acc + w) 0.0
    let gapsWidth := if layer.size > 1 then (layer.size - 1).toFloat * config.nodeGap else 0.0
    widthSum + gapsWidth

  let maxLayerWidth := layerTotalWidths.foldl max 0.0

  -- Track cumulative y position
  let mut currentY := config.padding

  for layerIdx in [0:s.layerNodes.size] do
    let layer := s.layerNodes[layerIdx]!
    let layerTotalWidth := layerTotalWidths[layerIdx]!

    -- Calculate horizontal centering offset for this layer
    let horizontalOffset := (maxLayerWidth - layerTotalWidth) / 2.0

    -- Place nodes in this layer horizontally
    let mut currentX := config.padding + horizontalOffset
    for nodeIdx in [0:layer.size] do
      let nodeId := layer[nodeIdx]!
      let nodeWidth := nodeWidths.get? nodeId |>.getD config.nodeWidth
      positions := positions.insert nodeId (currentX, currentY)
      currentX := currentX + nodeWidth + config.nodeGap

    -- Move y position for next layer
    currentY := currentY + config.nodeHeight + config.layerGap

  set { s with positions, nodeWidths }

/-- Assign coordinates using DOT-style algorithm:
    1. Initial grid-based placement
    2. Iterative refinement toward neighbor medians
    3. Overlap resolution -/
def assignCoordinates (g : Graph) (config : LayoutConfig) : LayoutM Unit := do
  -- Step 1: Initial grid-based placement
  assignInitialCoordinates g config

  -- Step 2: Refine positions toward neighbor medians (5 iterations)
  refinePositions g config 5

/-- Create layout edges with simple bezier control points (no obstacle avoidance).
    - Calculate line from center of source to center of target
    - Clip to source boundary for start point
    - Clip to target boundary for end point
    - Smooth bezier curves using offset-based control points -/
def createLayoutEdgesSimple (g : Graph) (config : LayoutConfig) : LayoutM (Array LayoutEdge) := do
  let s ← get
  let mut layoutEdges : Array LayoutEdge := #[]

  -- Build a lookup from node ID to Graph.Node (for shape info)
  let nodeMap : Std.HashMap String Node := g.nodes.foldl
    (fun acc node => acc.insert node.id node) {}

  for edge in g.edges do
    match (s.positions.get? edge.from_, s.positions.get? edge.to,
           nodeMap.get? edge.from_, nodeMap.get? edge.to) with
    | (some (x1, y1), some (x2, y2), some sourceNode, some targetNode) =>
      -- Get dynamic widths
      let sourceWidth := s.nodeWidths.get? edge.from_ |>.getD config.nodeWidth
      let targetWidth := s.nodeWidths.get? edge.to |>.getD config.nodeWidth

      -- Build LayoutNode structures for clipping
      let sourceLayoutNode : LayoutNode := {
        node := sourceNode
        x := x1
        y := y1
        width := sourceWidth
        height := config.nodeHeight
      }
      let targetLayoutNode : LayoutNode := {
        node := targetNode
        x := x2
        y := y2
        width := targetWidth
        height := config.nodeHeight
      }

      -- Get node centers
      let sourceCx := x1 + sourceWidth / 2
      let sourceCy := y1 + config.nodeHeight / 2
      let targetCx := x2 + targetWidth / 2
      let targetCy := y2 + config.nodeHeight / 2

      -- Clip to boundaries using center-to-center line
      let (startX, startY) := clipToNodeBoundary sourceLayoutNode targetCx targetCy
      let (endX, endY) := clipToNodeBoundary targetLayoutNode sourceCx sourceCy

      -- Smooth bezier: control points aligned with center-to-center direction
      -- This ensures arrowheads point toward the target node center
      let dx := targetCx - sourceCx
      let dy := targetCy - sourceCy
      let dist := Float.sqrt (dx * dx + dy * dy)

      -- Handle edge case where source and target are at the same position
      let points := if dist < 0.001 then
        #[(startX, startY), (startX, startY), (endX, endY), (endX, endY)]
      else
        -- Normalize direction and compute offset
        let dirX := dx / dist
        let dirY := dy / dist
        let offset := dist / 4.0

        -- Perpendicular vector for curve bulge (rotated 90 degrees)
        let perpX := -dirY
        let perpY := dirX

        -- Subtle curve amount for gentle arcs
        let curveAmount := min 8.0 (dist / 10.0)

        -- Control points with perpendicular offset for visible curve
        let cp1X := startX + dirX * offset + perpX * curveAmount
        let cp1Y := startY + dirY * offset + perpY * curveAmount

        let cp2X := endX - dirX * offset + perpX * curveAmount
        let cp2Y := endY - dirY * offset + perpY * curveAmount

        #[(startX, startY), (cp1X, cp1Y), (cp2X, cp2Y), (endX, endY)]
      layoutEdges := layoutEdges.push { from_ := edge.from_, to := edge.to, points, style := edge.style }
    | _ => pure ()

  return layoutEdges

/-- Create layout edges with obstacle-avoiding spline routing.
    Uses visibility graph + Dijkstra + Bezier fitting for smooth curves around nodes. -/
def createLayoutEdges (g : Graph) (config : LayoutConfig) : LayoutM (Array LayoutEdge) := do
  let s ← get
  let mut layoutEdges : Array LayoutEdge := #[]

  -- Build a lookup from node ID to Graph.Node (for shape info)
  let nodeMap : Std.HashMap String Node := g.nodes.foldl
    (fun acc node => acc.insert node.id node) {}

  -- Build obstacles from all nodes (with their positions and dimensions)
  -- Also build a map from node ID to obstacle index for exclusion
  let mut obstacles : Array Obstacle := #[]
  let mut nodeIdToObsIdx : Std.HashMap String Nat := {}

  for node in g.nodes do
    match s.positions.get? node.id with
    | some (x, y) =>
      let width := s.nodeWidths.get? node.id |>.getD config.nodeWidth
      let obsIdx := obstacles.size
      nodeIdToObsIdx := nodeIdToObsIdx.insert node.id obsIdx
      obstacles := obstacles.push {
        x := x
        y := y
        w := width
        h := config.nodeHeight
        shape := node.shape
      }
    | none => pure ()

  -- Route each edge
  for edge in g.edges do
    match (s.positions.get? edge.from_, s.positions.get? edge.to,
           nodeMap.get? edge.from_, nodeMap.get? edge.to) with
    | (some (x1, y1), some (x2, y2), some sourceNode, some targetNode) =>
      -- Get dynamic widths
      let sourceWidth := s.nodeWidths.get? edge.from_ |>.getD config.nodeWidth
      let targetWidth := s.nodeWidths.get? edge.to |>.getD config.nodeWidth

      -- Build LayoutNode structures for clipping
      let sourceLayoutNode : LayoutNode := {
        node := sourceNode
        x := x1
        y := y1
        width := sourceWidth
        height := config.nodeHeight
      }
      let targetLayoutNode : LayoutNode := {
        node := targetNode
        x := x2
        y := y2
        width := targetWidth
        height := config.nodeHeight
      }

      -- Get node centers
      let sourceCx := x1 + sourceWidth / 2
      let sourceCy := y1 + config.nodeHeight / 2
      let targetCx := x2 + targetWidth / 2
      let targetCy := y2 + config.nodeHeight / 2

      let sourceCenter : Point := ⟨sourceCx, sourceCy⟩
      let targetCenter : Point := ⟨targetCx, targetCy⟩

      -- Find indices of source and target obstacles to exclude them from collision
      let sourceObsIdx := nodeIdToObsIdx.get? edge.from_
      let targetObsIdx := nodeIdToObsIdx.get? edge.to
      let excludeIdxs := [sourceObsIdx, targetObsIdx].filterMap id |>.toArray

      -- Build visibility graph with source/target centers and all obstacle corners
      let vertices := collectVisibilityVertices sourceCenter targetCenter obstacles
      let visEdges := buildVisibilityGraph vertices obstacles excludeIdxs

      -- Source is index 0, target is last index (vertices.size - 1)
      let sourceIdx : Nat := 0
      let targetIdx : Nat := vertices.size - 1

      -- Find shortest path using Dijkstra
      let path := dijkstraShortestPath vertices visEdges sourceIdx targetIdx
      let pathPoints := getPathPoints vertices path

      -- Determine final waypoints
      let waypoints := if pathPoints.isEmpty || pathPoints.size < 2 then
        -- Fallback to direct line if no path found
        #[sourceCenter, targetCenter]
      else
        pathPoints

      -- Convert polyline to smooth Bezier curve
      let smoothPoints := polylineToBezier waypoints

      -- Get the second point (or target if only 2 points) for clipping direction
      let secondPoint := if smoothPoints.size > 1 then smoothPoints[1]! else targetCenter
      let secondLastPoint := if smoothPoints.size > 1 then smoothPoints[smoothPoints.size - 2]! else sourceCenter

      -- Clip start and end to node boundaries
      let (startX, startY) := clipToNodeBoundary sourceLayoutNode secondPoint.x secondPoint.y
      let (endX, endY) := clipToNodeBoundary targetLayoutNode secondLastPoint.x secondLastPoint.y

      -- Build final points array: replace first and last points with clipped versions
      let finalPoints : Array (Float × Float) :=
        if smoothPoints.size <= 2 then
          -- Simple line or very short path: create bezier with control points
          let dx := endX - startX
          let dy := endY - startY
          let dist := Float.sqrt (dx * dx + dy * dy)
          if dist < 0.001 then
            #[(startX, startY), (startX, startY), (endX, endY), (endX, endY)]
          else
            let offset := dist / 4.0
            let dirX := dx / dist
            let dirY := dy / dist

            -- Perpendicular vector for curve bulge
            let perpX := -dirY
            let perpY := dirX

            -- Subtle curve amount for gentle arcs
            let curveAmount := min 8.0 (dist / 10.0)

            let cp1 := (startX + dirX * offset + perpX * curveAmount,
                        startY + dirY * offset + perpY * curveAmount)
            let cp2 := (endX - dirX * offset + perpX * curveAmount,
                        endY - dirY * offset + perpY * curveAmount)
            #[(startX, startY), cp1, cp2, (endX, endY)]
        else
          -- Multi-point path: replace endpoints with clipped versions
          let pts := smoothPoints.map Point.toTuple
          let pts := pts.set! 0 (startX, startY)
          pts.set! (pts.size - 1) (endX, endY)

      layoutEdges := layoutEdges.push {
        from_ := edge.from_
        to := edge.to
        points := finalPoints
        style := edge.style
      }
    | _ => pure ()

  return layoutEdges

end Algorithm

/-- Perform complete layout of a graph -/
def layout (g : Graph) (config : LayoutConfig := {}) : LayoutGraph := Id.run do
  let (_, state) := (do
    Algorithm.assignLayers g
    Algorithm.orderLayers g config.barycenterIterations
    Algorithm.assignCoordinates g config
  ).run {}

  -- Create layout nodes with dynamic widths
  let mut layoutNodes : Array LayoutNode := #[]
  for node in g.nodes do
    match state.positions.get? node.id with
    | some (x, y) =>
      let width := state.nodeWidths.get? node.id |>.getD (computeNodeWidth config node.label)
      layoutNodes := layoutNodes.push {
        node
        x, y
        width
        height := config.nodeHeight
      }
    | none => pure ()

  -- Create layout edges
  let (layoutEdges, _) := Algorithm.createLayoutEdges g config |>.run state

  -- Calculate dimensions
  let maxX := layoutNodes.foldl (fun acc n => max acc (n.x + n.width)) 0.0
  let maxY := layoutNodes.foldl (fun acc n => max acc (n.y + n.height)) 0.0

  return {
    nodes := layoutNodes
    edges := layoutEdges
    width := maxX + config.padding
    height := maxY + config.padding
  }

end Dress.Graph.Layout
