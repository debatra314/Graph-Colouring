# 1.1 Implementation of Greedy Coloruing on Petersen Graph
create_petersen <- function() {
    edges <- matrix(c(
        0,1, 1,2, 2,3, 3,4, 4,0,      # inner 5-cycle
        5,6, 6,7, 7,8, 8,9, 9,5,      # outer 5-cycle
        0,5, 1,6, 2,7, 3,8, 4,9       # spokes
    ), ncol=2, byrow=TRUE)
    
    G <- vector("list", 10)
    for (i in 1:10) G[[i]] <- integer(0)
    for (i in 1:nrow(edges)) {
        u <- edges[i,1] + 1
        v <- edges[i,2] + 1
        G[[u]] <- unique(c(G[[u]], v))
        G[[v]] <- unique(c(G[[v]], u))
    }
    G
}

greedy_coloring <- function(G) {
    color <- rep(0, length(G))
    for (v in seq_along(G)) {
        used <- unique(color[G[[v]]])
        c <- 1
        while (c %in% used) c <- c + 1
        color[v] <- c
    }
    color
}

# Run
G <- create_petersen()
start <- Sys.time()
color <- greedy_coloring(G)
end <- Sys.time()

cat("---- Greedy Coloring ----\n")
cat("Colors used :", max(color), "\n")
cat("Time taken  :", as.numeric(end - start, units='secs'), "seconds\n")
cat("Memory used :", object.size(G) + object.size(color), "bytes\n")





# 1.2 Implementation of Welsh-Powel on Petersen Graph 

create_petersen <- function() {
    edges <- matrix(c(
        0,1, 1,2, 2,3, 3,4, 4,0,
        5,6, 6,7, 7,8, 8,9, 9,5,
        0,5, 1,6, 2,7, 3,8, 4,9
    ), ncol=2, byrow=TRUE)
    
    G <- vector("list", 10)
    for (i in 1:10) G[[i]] <- integer(0)
    for (i in 1:nrow(edges)) {
        u <- edges[i,1] + 1
        v <- edges[i,2] + 1
        G[[u]] <- unique(c(G[[u]], v))
        G[[v]] <- unique(c(G[[v]], u))
    }
    G
}

welsh_powell <- function(G) {
    order <- order(sapply(G, length), decreasing=TRUE)
    color <- rep(0, length(G))
    for (v in order) {
        used <- unique(color[G[[v]]])
        c <- 1
        while (c %in% used) c <- c + 1
        color[v] <- c
    }
    color
}

# Run
G <- create_petersen()
start <- Sys.time()
color <- welsh_powell(G)
end <- Sys.time()

cat("---- Welsh–Powell Algorithm ----\n")
cat("Colors used :", max(color), "\n")
cat("Time taken  :", as.numeric(end - start, units='secs'), "seconds\n")
cat("Memory used :", object.size(G) + object.size(color), "bytes\n")


# 1.3 Implementation of DSATUR on petersen graph

create_petersen <- function() {
    edges <- matrix(c(
        0,1, 1,2, 2,3, 3,4, 4,0,
        5,6, 6,7, 7,8, 8,9, 9,5,
        0,5, 1,6, 2,7, 3,8, 4,9
    ), ncol=2, byrow=TRUE)
    
    G <- vector("list", 10)
    for (i in 1:10) G[[i]] <- integer(0)
    for (i in 1:nrow(edges)) {
        u <- edges[i,1] + 1
        v <- edges[i,2] + 1
        G[[u]] <- unique(c(G[[u]], v))
        G[[v]] <- unique(c(G[[v]], u))
    }
    G
}

dsatur <- function(G) {
    n <- length(G)
    color <- rep(0, n)
    saturation <- rep(0, n)
    degree <- sapply(G, length)
    
    while (sum(color == 0) > 0) {
        uncolored <- which(color == 0)
        v <- uncolored[which.max(saturation[uncolored] * 100 + degree[uncolored])]
        used <- unique(color[G[[v]]])
        c <- 1
        while (c %in% used) c <- c + 1
        color[v] <- c
        for (nbor in G[[v]]) {
            if (color[nbor] == 0) {
                saturation[nbor] <- length(unique(color[G[[nbor]]][color[G[[nbor]]] != 0]))
            }
        }
    }
    color
}

# Run
G <- create_petersen()
start <- Sys.time()
color <- dsatur(G)
end <- Sys.time()

cat("---- DSatur Algorithm ----\n")
cat("Colors used :", max(color), "\n")
cat("Time taken  :", as.numeric(end - start, units='secs'), "seconds\n")
cat("Memory used :", object.size(G) + object.size(color), "bytes\n")



# 4.2 Applying Greedy colouring on small graphs (Hexagonal Graph with one diagonal, W_4, Tadpole Graph)

[language=Python, caption={Greedy graph coloring (Welsh--Powell) with actual time and space usage}, label={lst:greedy_coloring_actual}]
import time

def greedy_coloring(graph):
    """
    Greedy coloring of a graph represented as an adjacency dictionary.
    Vertices are colored in descending order of degree (Welsh--Powell style).
    """
    color_assignment = {}
    sorted_vertices = sorted(graph, key=lambda x: len(graph[x]), reverse=True)
    
    for vertex in sorted_vertices:
        neighbor_colors = {color_assignment.get(n) for n in graph[vertex] if n in color_assignment}
        color = 0
        while color in neighbor_colors:
            color += 1
        color_assignment[vertex] = color
    
    return color_assignment


def run_coloring(graphs):
    for name, graph in graphs:
        start = time.time()
        colors = greedy_coloring(graph)
        end = time.time()
        
        num_colors = max(colors.values()) + 1
        # Approximate space usage in bytes
        approx_space = sum(len(neigh) for neigh in graph.values()) + len(graph) + len(colors)
        
        print(f"Graph: {name}")
        print(f"Vertices: {len(graph)}, Edges: {sum(len(neigh) for neigh in graph.values()) // 2}")
        print(f"Colors used: {num_colors}")
        print(f"Time taken: {end - start:.6f} seconds")
        print(f"Approx. memory usage (number of items): {approx_space}")
        print("Color assignment:", colors)
        print("-" * 60)


# -------------------------------
# Example graphs
# -------------------------------

G_hex = {0:[1,5,2],1:[0,2],2:[1,3,0],3:[2,4],4:[3,5],5:[4,0]}
G_tadpole = {0:[1,2,3],1:[0,2],2:[0,1],3:[0,4],4:[3]}
G_wheel = {0:[1,2,3,4],1:[0,2,4],2:[0,1,3],3:[0,2,4],4:[0,1,3]}

graphs = [
    ("Hexagon with triangle", G_hex),
    ("Tadpole graph", G_tadpole),
    ("Wheel W4 graph", G_wheel)
]

run_coloring(graphs)








# 5.1 Implementing code to solve real life problem (Exam Scheduling 1)

[language=R, caption={Greedy Graph Coloring in R for Exam Scheduling Dataset}]


library(igraph)

file_path <- "C:/Users/debatra/OneDrive/Desktop/Exam Scheduling.csv"
data <- read.csv(file_path, header = TRUE)


if (nrow(data) == ncol(data)) {
  cat("Detected adjacency matrix format.\n")
  adj_matrix <- as.matrix(data)
  g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
} else {
  cat("Detected edge list format.\n")
  colnames(data)[1:2] <- c("from", "to")
  g <- graph_from_data_frame(data, directed = FALSE)
}


greedy_coloring <- function(g) {
  n <- vcount(g)
  colors <- rep(NA, n)
  degrees <- degree(g)
  order_vertices <- order(degrees, decreasing = TRUE)
  
  for (v in order_vertices) {
    neighbor_colors <- colors[neighbors(g, v)]
    color <- 1
    while (color %in% neighbor_colors) {
      color <- color + 1
    }
    colors[v] <- color
  }
  return(colors)
}

vertex_colors <- greedy_coloring(g)


num_colors <- max(vertex_colors)
cat("Number of colors used:", num_colors, "\n")
cat("Color assignment (vertex : color):\n")
print(data.frame(Vertex = V(g)$name, Color = vertex_colors))

plot(
  g,
  vertex.color = vertex_colors,
  vertex.label = V(g)$name,
  main = "Exam Scheduling via Greedy Graph Coloring"
)

# 5.2 Exam Scheduling Dataset 2

[language=R, caption={Greedy Graph Coloring for Exam Scheduling}]

library(data.table)
library(igraph)

df <- fread("C:/Users/debatra/OneDrive/Desktop/Exam SC2.csv")

df[, vertex_id := paste0(classroom_id, "_", building_name)]

edges_list <- lapply(unique(df$building_name), function(bld) {
  rooms <- df[building_name == bld, vertex_id]
  if(length(rooms) < 2) return(NULL)
  t(combn(rooms, 2))  # all pairs of classrooms in the building
})
edges <- do.call(rbind, edges_list)
edges <- as.data.table(edges)
setnames(edges, c("from", "to"))

g <- graph_from_data_frame(edges, vertices = df$vertex_id, directed = FALSE)

colors <- integer(vcount(g))
names(colors) <- V(g)$name

for(v in V(g)$name) {
  neighbor_colors <- colors[neighbors(g, v)$name]
  color <- 1
  while(color %in% neighbor_colors) color <- color + 1
  colors[v] <- color
}

result <- data.table(
  classroom_id = df$classroom_id,
  building = df$building_name,
  room_number = df$room_number,
  room_type = df$room_type,
  capacity = df$capacity,
  time_slot = colors[df$vertex_id]
)
result <- result[order(building, classroom_id)]
print(result)

plot(
  g,
  vertex.label = paste(df$room_number, df$building_name, sep = "-"),
  vertex.color = rainbow(max(colors))[colors],
  vertex.size = 25,
  vertex.label.cex = 1.2,
  main = "Exam Scheduling via Graph Coloring"
)



# 5.5 Greedy coloring of Europe Map

[language=R, caption={Greedy Coloring of Europe Map}, label={code:europe_greedy}]
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(spdep)
library(igraph)
library(ggplot2)

europe <- ne_countries(continent = "Europe", scale = "medium", returnclass = "sf")
nb <- poly2nb(europe, queen = TRUE)
edges <- nb2mat(nb, style = "B", zero.policy = TRUE)
g <- graph_from_adjacency_matrix(edges, mode = "undirected")

n <- vcount(g)
colors <- rep(0, n)
for (v in 1:n) {
  neighbors <- neighbors(g, v)
  neighbor_colors <- colors[as.numeric(neighbors)]
  color <- 1
  while (color %in% neighbor_colors) {
    color <- color + 1
  }
  colors[v] <- color
}

palette <- c("red", "blue", "green", "yellow", "orange", "purple", "cyan", "magenta")
europe$color <- palette[(colors %% length(palette)) + 1]

ggplot(europe) +
  geom_sf(aes(fill = color), color = "black") +
  scale_fill_identity() +
  ggtitle("Greedy Coloring of Europe Map") +
  theme_void() +
  theme(plot.title = element_text(size = 18, hjust = 0.5))







  # 5.6 Greedy Coloring of Asia Map

  [language=R, caption={Greedy Coloring of Asia Map}, label={code:asia_greedy}]
# ------------------------------
# Greedy Coloring of Asia Map
# ------------------------------

# Install required packages if not already installed
required_packages <- c("sf","rnaturalearth","spdep","igraph","ggplot2")
for(pkg in required_packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load libraries
library(sf)
library(rnaturalearth)
library(spdep)
library(igraph)
library(ggplot2)

asia <- ne_countries(continent = "Asia", scale = "medium", returnclass = "sf")


nb <- poly2nb(asia, queen = TRUE)


adj_matrix <- nb2mat(nb, style = "B", zero.policy = TRUE)


g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected")


n <- vcount(g)
colors <- rep(0, n)


for (v in 1:n) {
  neighbor_colors <- colors[as.numeric(neighbors(g, v))]
  color <- 1
  while (color %in% neighbor_colors) {
    color <- color + 1
  }
  colors[v] <- color
}


palette <- c("red", "blue", "green", "yellow", "orange", "purple", "cyan", "magenta")
asia$color <- palette[(colors %% length(palette)) + 1]


ggplot(asia) +
  geom_sf(aes(fill = color), color = "black") +
  scale_fill_identity() +
  ggtitle("Greedy Coloring of Asia Map") +
  theme_void() +
  theme(plot.title = element_text(size = 18, hjust = 0.5))

 Optional: save plot
 ggsave("asia_colored.png", width = 10, height = 8)

















 # 5.11 Frequency Assignment Via Graph Coloring Dataset 1

 [language=R, caption={Frequency Assignment via Graph Coloring in R}]



library(data.table)
library(igraph)


set.seed(42) 
n <- 150

df <- data.table(
  transmitter_id = 1:n,
  transmitter_name = paste0(
    toupper(substr(sample(LETTERS, n, replace=TRUE),1,1)),
    sapply(1:n, function(x) paste0(sample(letters, sample(4:7,1), replace=TRUE), collapse="")),
    " ",
    toupper(substr(sample(LETTERS, n, replace=TRUE),1,1)),
    sapply(1:n, function(x) paste0(sample(letters, sample(4:7,1), replace=TRUE), collapse=""))
  ),
  x_coord = runif(n, 0, 100),  # x coordinate in km
  y_coord = runif(n, 0, 100),  # y coordinate in km
  max_power = sample(c(5,10,15,20), n, replace = TRUE),  # in watts
  interference_radius = sample(c(2,3,4,5), n, replace = TRUE)  # in km
)


library(openxlsx)
write.xlsx(df, "C:/Users/debatra/OneDrive/Desktop/Frequency_Assignment.xlsx")


get_edges <- function(df) {
  edges <- list()
  n <- nrow(df)
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      dx <- df$x_coord[i] - df$x_coord[j]
      dy <- df$y_coord[i] - df$y_coord[j]
      dist <- sqrt(dx^2 + dy^2)
      if(dist <= (df$interference_radius[i] + df$interference_radius[j])) {
        edges <- append(edges, list(c(df$transmitter_id[i], df$transmitter_id[j])))
      }
    }
  }
  edges_df <- as.data.table(do.call(rbind, edges))
  setnames(edges_df, c("from","to"))
  return(edges_df)
}

edges <- get_edges(df)


g <- graph_from_data_frame(edges, vertices = df$transmitter_id, directed = FALSE)


colors <- integer(vcount(g))
names(colors) <- V(g)$name

for(v in V(g)$name) {
  neighbor_colors <- colors[neighbors(g, v)$name]
  color <- 1
  while(color %in% neighbor_colors) color <- color + 1
  colors[v] <- color
}


result <- data.table(
  transmitter_id = df$transmitter_id,
  transmitter_name = df$transmitter_name,
  x_coord = df$x_coord,
  y_coord = df$y_coord,
  max_power = df$max_power,
  interference_radius = df$interference_radius,
  assigned_frequency = colors[df$transmitter_id]
)


result <- result[order(assigned_frequency, transmitter_id)]
print(result)


plot(
  g,
  vertex.label = df$transmitter_name,
  vertex.color = rainbow(max(colors))[colors],
  vertex.size = 5,
  vertex.label.cex = 0.7,
  main = "Frequency Assignment via Graph Coloring"
)





# 5.14 Frequency Assignment Dataset 2



[language=R, caption={Frequency Assignment via Graph Coloring in R}]

library(data.table)
library(igraph)

df <- fread("C:/Users/debatra/OneDrive/Desktop/Generated_Frequency_Dataset.csv")


get_edges <- function(df){
  edges <- list()
  n <- nrow(df)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      dx <- df$x_coord[i] - df$x_coord[j]
      dy <- df$y_coord[i] - df$y_coord[j]
      dist <- sqrt(dx^2 + dy^2)
      if(dist <= (df$interference_radius[i] + df$interference_radius[j])){
        edges <- append(edges, list(c(df$transmitter_id[i], df$transmitter_id[j])))
      }
    }
  }
  edges_df <- as.data.table(do.call(rbind, edges))
  setnames(edges_df, c("from","to"))
  return(edges_df)
}

edges <- get_edges(df)


g <- graph_from_data_frame(edges, vertices = df$transmitter_id, directed = FALSE)

colors <- integer(vcount(g))
names(colors) <- V(g)$name
for(v in V(g)$name){
  neighbor_colors <- colors[neighbors(g, v)$name]
  color <- 1
  while(color %in% neighbor_colors) color <- color + 1
  colors[v] <- color
}


result <- data.table(
  transmitter_id = df$transmitter_id,
  transmitter_name = df$transmitter_name,
  x_coord = df$x_coord,
  y_coord = df$y_coord,
  interference_radius = df$interference_radius,
  assigned_frequency = colors[df$transmitter_id]
)


fwrite(result, "C:/Users/debatra/OneDrive/Desktop/Frequency_Assignment_Results.csv")
