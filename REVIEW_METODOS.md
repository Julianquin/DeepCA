# Revisión: ¿son estos métodos "clustering profundo no paramétrico en K"?

Revisión de los 7 papers en `papers/` contra las 6 implementaciones en `models/`.

**Resultado corto: la premisa no se sostiene para 3 de los 6 métodos, y el único
paper que encaja perfecto con lo que buscas no tiene código en el repo.**

| Modelo | ¿Infiere K? | ¿Deep learning? | Veredicto |
|---|---|---|---|
| DEC | ❌ No | ✅ Sí | Paramétrico. Sobra si el criterio es "no paramétrico". |
| VaDE | ❌ No | ✅ Sí | Paramétrico. Ídem. |
| DSC-Net | ❌ No | ✅ Sí | Paramétrico. Ídem. |
| DeepDPM | ✅ Sí | ✅ Sí | **El único que cumple las dos cosas.** |
| SB-VAE | ⚠️ En otro eje | ✅ Sí | No paramétrico en *dimensión latente*, no en clusters. Ni siquiera es un método de clustering. |
| LSPCM | ✅ Sí | ❌ No (MCMC) | No paramétrico de verdad, pero bayesiano y solo para redes. |
| DPGMM + α-JS | ✅ Sí | ✅ Sí | **Encaja perfecto — pero no hay código en el repo.** |

---

## Los tres que no cumplen

### DEC (Xie et al. 2016)

K es un hiperparámetro. El paper lo dice explícitamente al describir los
experimentos: *"set number of clusters to the number of ground-truth
categories"*. `models/dec/run_dec.py` exige `--n_clusters` (obligatorio).

Matiz: la §5.5 ("Number of Clusters") sí propone elegir K, pero **barriendo
K = 3…19 y quedándose con el codo de una métrica de generalizabilidad**
(`G = L_train / L_val`). Eso es *selección de modelo por fuerza bruta*, no
inferencia no paramétrica: entrenas un modelo por cada K. De hecho en MNIST el
criterio elige K=9, no 10.

### VaDE (Jiang et al. 2017)

Es un VAE cuyo prior es una mezcla de gaussianas con **K fijo** (§3.1: *"donde K
es un parámetro predefinido"*). §4.2: *"The number of clusters is fixed to the
number of classes for each dataset, similar to DEC"*. La §4.6 solo enseña qué
pasa si te equivocas de K (con K=7 y K=14 en MNIST), no lo infiere.
`run_vade.py` exige `--n_clusters`.

### DSC-Net (Ji et al. 2017)

Aprende una matriz de auto-expresión N×N con una capa "self-expressive", la
convierte en matriz de afinidad y luego aplica **spectral clustering**, que
necesita K. El paper mide "robustez al número de clusters" probando
combinaciones de 10/15/…/38 sujetos, pero K se le pasa siempre.
`run_dscnet.py` exige `--n_clusters`.

Aparte, escala O(N²) en memoria: es inviable más allá de unos pocos miles de
muestras (por eso el paper solo usa datasets de 400–7200 imágenes).

---

## Los que sí (con matices)

### DeepDPM (Ronen et al. 2022) — el que realmente buscas

Mezcla de procesos de Dirichlet con propuestas de *split/merge* sobre los
clusters durante el entrenamiento: K cambia dinámicamente. El título del paper
es literalmente *"Deep Clustering With an Unknown Number of Clusters"*.
`run_deepdpm.py` es el único wrapper que **no** tiene `--n_clusters`.

Es también el único de los 6 cuyo repo oficial trae los embeddings
preentrenados con los que se generaron las tablas del paper — ya descargados en
`data/deepdpm_embeddings/`.

### LSPCM (Gwee et al.) — no paramétrico, pero no es deep learning

Infiere **dos** cosas a la vez: la dimensión efectiva del espacio latente (prior
de shrinkage MTGP, espacio latente infinito-dimensional) y el número de clusters
(*sparse finite mixture*, donde G=20 es un techo y el modelo decide cuántas
componentes quedan no vacías). Eso es genuinamente no paramétrico.

Dos limitaciones que conviene tener claras:

1. **No es una red neuronal.** Es un muestreador Metropolis-within-Gibbs. No hay
   gradientes, ni GPU, ni representación aprendida. El `README.md` ya lo dice.
2. **Solo acepta redes/grafos**, no vectores de features. Si tus datos no son un
   grafo, LSPCM no aplica en absoluto.

### SB-VAE (Nalisnick & Smyth 2017) — está clasificado mal

Esto es lo que más conviene corregir. El *stick-breaking* del SB-VAE es sobre la
**dimensionalidad del espacio latente**, no sobre clusters: el modelo decide
cuántas dimensiones latentes usar, y nunca infiere un número de grupos. El paper
no hace clustering en ningún experimento — evalúa ELBO/densidad en Frey Faces,
MNIST, MNIST+rot y SVHN, y calidad discriminativa con un **kNN** sobre el
latente.

Y en el repo el problema se agrava: `run_sbvae.py` entrena el SB-VAE y después
le mete un **KMeans con `--n_clusters`** encima. O sea que tal como está
integrado, el método es *paramétrico en K* — el propio docstring del wrapper lo
admite (*"el número de clusters real se decide en el paso de K-means"*).

Sigue siendo útil como *baseline de representación* o como pieza de un modelo
mayor, pero no cuenta como método de clustering no paramétrico.

---

## Lo que falta: DPGMM + α-Jensen-Shannon (Lim)

`papers/Deep Clustering using Dirichlet Process Gaussian Mixture and Alpha
JensenShannon Divergence Clustering Loss.pdf` es el paper que mejor encaja con
tu criterio, y **no tiene carpeta en `models/`**.

Su abstract ataca exactamente los dos problemas que tienen DEC y VaDE:

> *"they rely on the mathematical convenience of Kullback-Leibler divergence for
> the clustering loss function but the former is asymmetric. Secondly, they
> assume the prior knowledge on the number of clusters is always available."*

Propone un autoencoder con una DP-GMM en el espacio latente ("deep model
selection"): el número de clusters varía durante el entrenamiento hasta
converger. Usa MNIST (píxeles crudos), CIFAR-10, MIT67 y CIFAR-100 (con features
de ResNet18 preentrenado), con niveles de truncamiento T = 50/100/200.

No parece haber implementación oficial pública. Es implementable —
autoencoder + DP-GMM variacional con truncamiento + pérdida α-JSD en vez de
KL — pero es trabajo de reimplementación desde el paper, no de clonar un repo.

---

## Estado real del código (independiente de lo anterior)

| Modelo | Estado |
|---|---|
| SB-VAE | Funcional (reimplementación propia en PyTorch) |
| DeepDPM | Funcional: `run_deepdpm.py` exporta tensores y te imprime el comando de `DeepDPM.py` |
| LSPCM | Código R completo, pero **R no está instalado en este sistema** (`Rscript` no existe) |
| DEC | `NotImplementedError` — falta conectar con la API de `ptdec` |
| DSC-Net | `NotImplementedError` — falta exportar a `.mat`. *(Ya resuelto en parte: `download_datasets.py` copia los `.mat` originales a `models/dscnet_src/datasets/`, así que `python main.py --db coil20` debería correr tal cual.)* |
| VaDE | `NotImplementedError` — falta escribir un `LightningDataModule` |

**Entorno:** el `README.md` manda `conda env create -f envs/environment_pytorch.yml`,
pero en este sistema no hay `conda` ni `pip` ni `R`. Se creó `.venv/` con
numpy/scipy/pandas/scikit-learn/pillow para el pipeline de datos; **falta
instalar `torch`, `torchvision` y `pytorch-lightning`** antes de poder entrenar
nada.

---

## Recomendación

Si el criterio es estricto ("deep clustering con K no paramétrico"), el repo se
reduce a **DeepDPM**, más **LSPCM** si tus datos son redes, más **DPGMM+α-JS** si
lo implementas.

Tres caminos, no excluyentes:

1. **Quedarte con DEC / VaDE / DSC-Net como baselines paramétricos.** Es lo
   habitual: DeepDPM los usa así en sus tablas (marcados con `p` = "requiere K").
   Es una comparación legítima y hace la evaluación más fuerte. Solo hay que
   dejar de llamarlos no paramétricos.
2. **Implementar DPGMM + α-JSD.** Es el hueco real del repo y el paper más
   alineado con tu objetivo.
3. **Reconsiderar SB-VAE.** O lo quitas, o lo reetiquetas como baseline de
   representación, o lo conviertes en algo no paramétrico de verdad
   reemplazando el KMeans final por una DP-GMM sobre el embedding.

Métodos que faltan y encajarían en el criterio, si quieres ampliar: DCC
(Shah & Koltun), VaDE con prior de proceso de Dirichlet, DeepECT (jerárquico,
sin K fijo), y SCAN/DipDECK (este último estima K con el test de Hartigan).
