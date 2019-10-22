# bwstools

Tools for Best-Worst Scaling (a.k.a., MaxDiff) Designs

Currently contains:

- Aggregate estimates, based on: analytical estimation of the multinomial logit model using `ae_mnl()`

- Individual estimates, based on: difference scores (best minus worst) using `diffscoring()`, random walks in directed networks using `walkscoring()`, and empirical Bayes using `e_bayescoring()`

- A data.frame of balanced incomplete block designs

Planned for 1.0 release:

- A function to generate a balanced incomplete block design from the data.frame of designs

- Individual estimates: based on Elo ratings
