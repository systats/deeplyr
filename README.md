# deeplyr

This is a light-weight model engine written in R mainly for text classification tasks. While Keras, XGBoost or Ranger (RF) provide high quality learners, `deeplyr` comes with unifying train/test/report functions for all backends mentioned. This straightforward data and model worklfow with various different backends is automatically logged (thanks to [`logger`](https://github.com/daroczig/logger)), benchmarked and can also be optimized over hyperparameter spaces. 
