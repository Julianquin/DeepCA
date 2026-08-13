# Wrapper de LSPCM para el suite -- YA CONECTADO a la API real del paquete
# `lspm` (v1.2.0, R/LSPCM.R + R/LSPCM_labels.R).
#
# A diferencia de los otros 5 modelos, LSPCM:
#   - No recibe vectores de características ni imágenes, sino una RED
#     (matriz de adyacencia). Úsalo solo si tus datos son de tipo grafo/red.
#   - Es Bayesiano vía MCMC (no una red neuronal entrenada por gradiente),
#     así que "iter/burnin/thin" son iteraciones de cadena de Markov, no
#     épocas.
#   - El número de clusters G es un techo (overfitted mixture): el modelo
#     infiere cuántos de esos G clusters quedan realmente activos, no hay
#     que acertarle al número exacto.
#
# Instalar el paquete (una sola vez):
#   install.packages(c("abind","mvtnorm","sna","truncdist","vioplot","Rfast",
#                       "MCMCpack","mclust","mcclust","rootSolve","seriation"))
#   # Desde la raíz de models/lspcm_src/:
#   remotes::install_local(dependencies = TRUE)
#
# Uso:
#   Rscript run_lspcm.R --adjacency ../../data/mi_red_adj.csv \
#       --n_dimen 5 --G 20 --iter 50000 --burnin 5000 --thin 100 \
#       --labels ../../data/labels_reales.csv   # opcional, solo para ARI

suppressMessages(library(lspm))

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- which(args == flag)
  if (length(i) == 0) return(default)
  args[i + 1]
}

adjacency_path <- get_arg("--adjacency")
labels_path    <- get_arg("--labels", NULL)
n_dimen        <- as.integer(get_arg("--n_dimen", "5"))
G              <- as.integer(get_arg("--G", "20"))
iter           <- as.numeric(get_arg("--iter", "5e4"))
burnin         <- as.numeric(get_arg("--burnin", "5e3"))
thin           <- as.numeric(get_arg("--thin", "100"))
out_dir        <- get_arg("--out", "../../results/lspcm")

if (is.null(adjacency_path)) {
  stop("Debes pasar --adjacency <ruta_a_matriz_de_adyacencia.csv>")
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- Cargar red -------------------------------------------------------------
# Espera una matriz de adyacencia N x N (la que produce
# common/data_loader.py::load_edgelist(), guardada sin encabezados).
A <- as.matrix(read.csv(adjacency_path, header = FALSE))
storage.mode(A) <- "double"
cat(sprintf("[LSPCM] red cargada: %d nodos, %d aristas (dirigidas)\n",
            nrow(A), sum(A != 0)))

# --- Ajustar el modelo --------------------------------------------------
# Nota de rendimiento: iter/thin por defecto (50k/100) puede tardar varios
# minutos en CPU incluso en redes pequeñas (n ~ decenas-cientos de nodos).
# Para una primera prueba rápida, baja --iter a 5000 y --thin a 20.
fit <- LSPCM(
  network    = A,
  G          = G,
  n_dimen    = n_dimen,
  iter       = iter,
  burnin     = burnin,
  thin       = thin
)

# --- Extraer partición representativa (PEAR) -----------------------------
true_labels <- NULL
if (!is.null(labels_path)) {
  true_labels <- read.csv(labels_path, header = FALSE)[[1]]
}

if (!is.null(true_labels)) {
  cluster_result <- LSPCM_labels(fit, type = "PEAR", true_labels = true_labels)
  cat(sprintf("[LSPCM] ARI vs. labels reales: %.4f\n", cluster_result$ARI_pear))
} else {
  cluster_result <- LSPCM_labels(fit, type = "PEAR")
}

pred_labels <- cluster_result$mpear
cat(sprintf("[LSPCM] número de clusters inferido: %d\n",
            length(unique(pred_labels))))

# --- Guardar resultados en el formato común del suite ---------------------
# preds.csv es leído por common/run_all.py (equivalente al preds.npy de los
# modelos Python) para comparar todos los modelos con el mismo criterio.
write.csv(pred_labels, file.path(out_dir, "preds.csv"),
          row.names = FALSE)
saveRDS(fit, file.path(out_dir, "lspcm_fit_full.rds"))  # cadena MCMC completa

cat(sprintf("[LSPCM] resultados guardados en %s\n", out_dir))
