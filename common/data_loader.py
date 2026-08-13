"""
Cargador de datos unificado para el suite de deep clustering.

Soporta tres tipos de entrada, según lo que necesita cada modelo:

1. Vectores / tabular  -> usado por DEC, DSC-Net, VaDE, SB-VAE, DeepDPM
   Formatos aceptados: .npy (array N x D), .csv (filas=muestras, cols=features)

2. Imágenes            -> usado por DEC, DSC-Net, VaDE, SB-VAE, DeepDPM
   Carpeta con subcarpetas por clase (solo para evaluación con labels reales,
   NO se usan las labels para entrenar) o carpeta plana de imágenes.
   Formatos aceptados: .png, .jpg

3. Red / grafo          -> usado SOLO por LSPCM (código R aparte, ver models/lspcm/)
   Formato aceptado: lista de aristas .csv (nodo_origen, nodo_destino[, peso])
   o matriz de adyacencia .csv / .npy (N x N)

Uso típico:

    from data_loader import load_tabular, load_images, load_edgelist

    X, y = load_tabular("data/mis_features.npy", labels_path="data/labels.npy")
    X, y = load_images("data/mis_imagenes/", image_size=(28, 28), grayscale=True)
    A, node_names = load_edgelist("data/mi_red.csv")
"""

from __future__ import annotations
import numpy as np
import pandas as pd
from pathlib import Path
from typing import Optional, Tuple, List


def load_tabular(
    path: str,
    labels_path: Optional[str] = None,
    standardize: bool = True,
) -> Tuple[np.ndarray, Optional[np.ndarray]]:
    """Carga datos tabulares (.npy o .csv) como matriz N x D (float32).

    Si labels_path se provee, se usan SOLO para evaluar (ACC/NMI/ARI),
    nunca para entrenar los modelos (todos son no supervisados).
    """
    path = Path(path)
    if path.suffix == ".npy":
        X = np.load(path)
    elif path.suffix == ".csv":
        X = pd.read_csv(path).values
    else:
        raise ValueError(f"Formato no soportado: {path.suffix}. Usa .npy o .csv")

    X = X.astype(np.float32)

    if standardize:
        mean = X.mean(axis=0, keepdims=True)
        std = X.std(axis=0, keepdims=True) + 1e-8
        X = (X - mean) / std

    y = None
    if labels_path is not None:
        lp = Path(labels_path)
        y = np.load(lp) if lp.suffix == ".npy" else pd.read_csv(lp).values.ravel()

    return X, y


def load_images(
    folder: str,
    image_size: Tuple[int, int] = (28, 28),
    grayscale: bool = True,
    flatten: bool = True,
) -> Tuple[np.ndarray, Optional[np.ndarray]]:
    """Carga imágenes desde una carpeta.

    Si la carpeta tiene subcarpetas (una por clase), se devuelven labels
    (solo para evaluación posterior, no para entrenar).
    Si no, se devuelve y=None.
    """
    from PIL import Image

    folder = Path(folder)
    subdirs = [d for d in folder.iterdir() if d.is_dir()]

    images: List[np.ndarray] = []
    labels: List[int] = []

    def _load_one(fp: Path) -> np.ndarray:
        img = Image.open(fp).convert("L" if grayscale else "RGB")
        img = img.resize(image_size)
        arr = np.asarray(img, dtype=np.float32) / 255.0
        return arr

    if subdirs:
        for class_idx, sub in enumerate(sorted(subdirs)):
            for fp in sorted(sub.glob("*")):
                if fp.suffix.lower() not in (".png", ".jpg", ".jpeg"):
                    continue
                images.append(_load_one(fp))
                labels.append(class_idx)
        y = np.array(labels)
    else:
        for fp in sorted(folder.glob("*")):
            if fp.suffix.lower() not in (".png", ".jpg", ".jpeg"):
                continue
            images.append(_load_one(fp))
        y = None

    X = np.stack(images, axis=0)
    if flatten:
        X = X.reshape(X.shape[0], -1)
    else:
        # formato N x C x H x W para modelos con CNN (DSC-Net principalmente)
        if grayscale:
            X = X[:, None, :, :]
        else:
            X = X.transpose(0, 3, 1, 2)

    return X.astype(np.float32), y


def load_edgelist(path: str) -> Tuple[np.ndarray, List[str]]:
    """Carga una red desde lista de aristas o matriz de adyacencia.

    Devuelve (matriz_adyacencia N x N, lista_de_nombres_de_nodo).
    Este formato es el que espera el wrapper de LSPCM (models/lspcm/).
    """
    path = Path(path)

    if path.suffix == ".npy":
        A = np.load(path)
        nodes = [str(i) for i in range(A.shape[0])]
        return A, nodes

    # Una matriz de adyacencia guardada SIN cabecera (que es el formato que
    # exige models/lspcm/run_lspcm.R, con read.csv(header = FALSE)) se leería
    # mal con el header por defecto de pandas: la primera fila se convertiría
    # en nombres de columna y la matriz dejaría de ser cuadrada. Se prueba
    # primero sin cabecera y solo se cae al modo con cabecera si eso no da una
    # matriz cuadrada numérica.
    df = pd.read_csv(path, header=None)
    if not (_looks_like_square_matrix(df) and _all_numeric(df)):
        df = pd.read_csv(path)

    # Para ser una matriz de adyacencia hay que ser cuadrada Y numérica: sin lo
    # segundo, una lista de aristas con nombres de nodo y tantas filas como
    # columnas (p.ej. 3 aristas x 3 columnas) se confundiría con una matriz.
    if df.shape[1] >= 2 and not (_looks_like_square_matrix(df) and _all_numeric(df)):
        # lista de aristas: col0=origen, col1=destino, col2(opcional)=peso
        nodes = sorted(set(df.iloc[:, 0]) | set(df.iloc[:, 1]))
        idx = {n: i for i, n in enumerate(nodes)}
        n = len(nodes)
        A = np.zeros((n, n), dtype=np.float32)
        weights = df.iloc[:, 2] if df.shape[1] > 2 else np.ones(len(df))
        for (a, b), w in zip(df.iloc[:, :2].values, weights):
            A[idx[a], idx[b]] = w
        return A, [str(n) for n in nodes]

    # matriz de adyacencia ya cuadrada
    A = df.values.astype(np.float32)
    nodes = [str(i) for i in range(A.shape[0])]
    return A, nodes


def _looks_like_square_matrix(df: pd.DataFrame) -> bool:
    return df.shape[0] == df.shape[1]


def _all_numeric(df: pd.DataFrame) -> bool:
    return all(pd.api.types.is_numeric_dtype(t) for t in df.dtypes)


if __name__ == "__main__":
    print(__doc__)
