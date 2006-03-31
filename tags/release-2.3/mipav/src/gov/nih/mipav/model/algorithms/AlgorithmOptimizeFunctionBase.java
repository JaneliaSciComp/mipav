package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;

public interface AlgorithmOptimizeFunctionBase {
    public double cost(double x[]);
    public double cost(TransMatrix tMatrix);
}