# Probability Distribution

Probability distribution functions which are inspired by R.

d - density
p - probability (cumulative)
q - quantile
r - random number generation

## Usage

```scala
// For single value
norm.dnorm(0.3, 0.0, 1.0, true)
// For a vector of values
norm.dnorm(List(0.3, 0.4, 0.5), 0.0, 1.0, true)
```
### Installation
```scala
git clone https://github.com/EstusDev/Estus
cd Estus/estus-distribution
sbt estusDistribution/update
sbt estusDistribution/compile
```

### Testing
Need unit tests
