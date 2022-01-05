package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;


/**
 * DOCUMENT ME!
 */
public interface AlgorithmOptimizeFunctionBase {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    double cost(double[] x);

    /**
     * DOCUMENT ME!
     *
     * @param   tMatrix  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    double cost(TransMatrix tMatrix);
    
    double cost(TransMatrixd tMatrix);
    
    int getCostFunction();
}
