# Numerical Optimization

SolverMOS is a metaheuristic which attempts to find the global optimum of an objective function. The variables of the objective function must be continuous.

**Key Features**
  - No assumptions about the objective function, e.g. no gradient required
  - Box constraints, equality constraints and inequality constraints
  - Highly scalable, non-blocking and fault tolerant
  - Designed for large-scale optimizations and achieved some very [competitive results](https://github.com/EstusDev/Estus/wiki/SolverMOS#performance-analysis) (global optimization on 1000 dimensional problems)
  
You can find more information on [SolverMOS's wiki page](https://github.com/EstusDev/Estus/wiki/SolverMOS).

## Usage

### Installation
```
git clone https://github.com/EstusDev/Estus
cd Estus/estus-optimization
sbt estusOptimization/update
sbt estusOptimization/compile
```

### Testing
```
cd Estus/estus-optimization
sbt estusOptimization/test
```
