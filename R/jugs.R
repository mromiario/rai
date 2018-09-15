library(data.tree)

# Jug A capacity
N <- 12

# Jug B capacity
M <- 11

# Desired result
X <- 6

# Starting point
S <- c(0, 0)

# User defined production rules as possible moves
possible_moves <- c(
  (function(p, q) c(12, q)),
  (function(p, q) c(p, 11)),
  (function(p, q) c(0, q)),
  (function(p, q) c(p, 0)),
  (function(p, q) if ((p + q) <= N) c(p + q, 0) else c(p + (N - p), q - (N - p))),
  (function(p, q) if ((q + p) <= M) c(0, q + p) else c(p - (M - q), q + (M - q)))
)

to_node <- function(x) paste0("(A = ", x[[1]], ", B = ", x[[2]], ")")

library(data.tree)

solution <- Node$new(to_node(S))

bfs <- function() {
  queue <- list()
  queue <- c(queue, list(S))

  visited <- list()

  childs <- list()
  childs[[to_node(S)]] <- solution

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]

    node <- to_node(current)
    visited[[node]] <- TRUE

    p <- current[[1]]
    q <- current[[2]]

    if (p == X || q == X) {
      queue <- list()
      next
    }

    for (next_move in possible_moves) {
      next_move <- next_move(p, q)
      next_node <- to_node(next_move)

      if (is.null(visited[[next_node]])) {
        queue <- c(queue, list(next_move))
        visited[[node]] <- TRUE

        childs[[next_node]] <- childs[[node]]$AddChild(next_node)
      }
    }
  }
}

# Run our BFS first
bfs()

# Styling our Graph plot
SetGraphStyle(solution, rankdir = "TB")
SetEdgeStyle(solution, arrowhead = "vee", color = "grey35", penwidth = 2)

SetNodeStyle(solution, style = "filled, rounded", shape = "box", fontcolor = "Black", fillcolor = "LightBlue", tooltip = GetDefaultTooltip)
Do(solution$leaves, function(node) SetNodeStyle(node, fontcolor = "White", fillcolor = "YellowGreen"))

# Display it. Using print instead of plot for CLI.
print(solution)

