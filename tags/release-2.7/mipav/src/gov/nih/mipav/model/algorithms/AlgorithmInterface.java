package gov.nih.mipav.model.algorithms;


/**
 * The interface used by all classes which want to respond to the conclusion of an algorithm. The algorithm may not have
 * completed sucessfully, so checking the value of <code>isCompleted()</code> may be necessary.
 *
 * @see      AlgorithmBase#isCompleted()
 * @version  0.1 November 24, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public interface AlgorithmInterface {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Called after an algorithm this listener is registered to exits (maybe successfully, maybe not). If the algorithm
     * is run in a separate thread, this call will be made within that thread. If not, this call will be made from that
     * same, shared thread.
     *
     * @param  algorithm  the algorithm which has just completed
     */
    void algorithmPerformed(AlgorithmBase algorithm);
}
