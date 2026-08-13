"""
Métricas comunes para comparar los 6 modelos con el mismo criterio.

Todas requieren labels reales (ground truth) para evaluar, aunque el
entrenamiento de los modelos sea no supervisado. Si no tienes labels,
puedes seguir usando estas funciones con métricas internas (silhouette)
en su lugar -- ver `silhouette` más abajo.
"""

from __future__ import annotations
import numpy as np
from scipy.optimize import linear_sum_assignment
from sklearn.metrics import normalized_mutual_info_score, adjusted_rand_score
from sklearn.metrics import silhouette_score


def cluster_accuracy(y_true: np.ndarray, y_pred: np.ndarray) -> float:
    """ACC con el mejor emparejamiento cluster<->clase (algoritmo húngaro)."""
    y_true = np.asarray(y_true).astype(np.int64)
    y_pred = np.asarray(y_pred).astype(np.int64)
    assert y_pred.size == y_true.size

    D = max(y_pred.max(), y_true.max()) + 1
    w = np.zeros((D, D), dtype=np.int64)
    for i in range(y_pred.size):
        w[y_pred[i], y_true[i]] += 1

    row_ind, col_ind = linear_sum_assignment(-w)
    return w[row_ind, col_ind].sum() / y_pred.size


def evaluate_all(y_true: np.ndarray, y_pred: np.ndarray) -> dict:
    """Devuelve ACC, NMI y ARI en un solo dict, listo para comparar modelos."""
    return {
        "ACC": cluster_accuracy(y_true, y_pred),
        "NMI": normalized_mutual_info_score(y_true, y_pred),
        "ARI": adjusted_rand_score(y_true, y_pred),
    }


def silhouette(X: np.ndarray, y_pred: np.ndarray) -> float:
    """Métrica interna, útil cuando NO tienes labels reales."""
    return silhouette_score(X, y_pred)


def print_report(model_name: str, metrics: dict) -> None:
    line = f"{model_name:12s} | " + " | ".join(
        f"{k}={v:.4f}" for k, v in metrics.items()
    )
    print(line)
