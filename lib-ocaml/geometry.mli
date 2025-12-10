
type segment

val make_segment : Coord.t -> Coord.t -> segment

val pr_segment : segment -> string

val is_on_segment : Coord.t -> segment -> bool

val project_point_on_line : Coord.t -> segment -> Coord.t

(** Returns true iff the interior of the segment intersect (does not manage the Collinear case) *)
val segment_intersect : segment -> segment -> bool

type polygon

(** Make a polygon from a list of points (warning: comes back to the first point) *)
val make_polygon : Coord.t list -> polygon

(** Checks whether a point is inside a polygon using raycasting *)
val is_inside_polygon : polygon -> Coord.t -> bool

type rectangle

(** Make a rectangle a diagonal *)
val make_rectangle : Coord.t -> Coord.t -> rectangle

val pr_rectangle : rectangle -> string

(** Checks if the given rectangle is inside the given polygon.
    It is inclusive, i.e., it returns true for rectangles that contain segments of the polygon *)
val is_rectangle_inside_polygon : polygon -> rectangle -> bool
