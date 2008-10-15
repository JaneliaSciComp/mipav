package gov.nih.mipav.view.renderer.J3D.model.structures;


/**
 * Used in ModelSurfaceDecimator and ModelSurfaceTopology.
 */

public class ModelSet {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Object[] m_atElement;

    /** DOCUMENT ME! */
    private int m_iCapacity, m_iGrowBy, m_iSize;

    //~ Constructors ---------------------------------------------------------------------------------------------------


    /**
     * Creates a new ModelSet object.
     */
    public ModelSet() {
        m_iCapacity = 1;
        m_iGrowBy = 1;
        m_iSize = 0;
        m_atElement = new Object[1];
    }

    /**
     * Creates a new ModelSet object.
     *
     * @param  kSet  DOCUMENT ME!
     */
    public ModelSet(ModelSet kSet) {

        // copy constructor
        m_iCapacity = kSet.m_iCapacity;
        m_iGrowBy = kSet.m_iGrowBy;
        m_iSize = kSet.m_iSize;
        m_atElement = new Object[m_iCapacity];
        System.arraycopy(kSet.m_atElement, 0, m_atElement, 0, m_iCapacity);
    }

    /**
     * Creates a new ModelSet object.
     *
     * @param  iCapacity  DOCUMENT ME!
     * @param  iGrowBy    DOCUMENT ME!
     */
    public ModelSet(int iCapacity, int iGrowBy) {

        // assert:  iCapacity > 0 && iGrowBy > 0
        m_iCapacity = iCapacity;
        m_iGrowBy = iGrowBy;
        m_iSize = 0;
        m_atElement = new Object[iCapacity];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param   kElement  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean add(Object kElement) {

        for (int i = 0; i < m_iSize; i++) {

            if (kElement.equals(m_atElement[i])) {
                return false;
            }
        }

        if (m_iSize == m_iCapacity) {

            // array is full, resize it
            int iNewCapacity = m_iCapacity + m_iGrowBy;
            Object[] atNewElement = new Object[iNewCapacity];
            System.arraycopy(m_atElement, 0, atNewElement, 0, m_iCapacity);
            m_atElement = atNewElement;
            m_iCapacity = iNewCapacity;
        }

        m_atElement[m_iSize++] = kElement;

        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  iCapacity  DOCUMENT ME!
     * @param  iGrowBy    DOCUMENT ME!
     */
    public void clear(int iCapacity, int iGrowBy) {

        // assert: iCapacity > 0 && iGrowBy > 0
        m_iCapacity = iCapacity;
        m_iGrowBy = iGrowBy;
        m_iSize = 0;
        m_atElement = new Object[iCapacity];
    }

    /**
     * DOCUMENT ME!
     *
     * @param   kElement  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean exists(Object kElement) {

        for (int i = 0; i < m_iSize; i++) {

            if (kElement.equals(m_atElement[i])) {
                return true;
            }
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   i  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Object get(int i) {

        if ((0 <= i) && (i < m_iSize)) {
            return m_atElement[i];
        } else {
            return null;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  kElement  DOCUMENT ME!
     */
    public void insertNoCheck(Object kElement) {

        if (m_iSize == m_iCapacity) {

            // array is full, resize it
            int iNewCapacity = m_iCapacity + m_iGrowBy;
            Object[] atNewElement = new Object[iNewCapacity];
            System.arraycopy(m_atElement, 0, atNewElement, 0, m_iCapacity);
            m_atElement = atNewElement;
            m_iCapacity = iNewCapacity;
        }

        m_atElement[m_iSize++] = kElement;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isEmpty() {
        return m_iSize == 0;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   kElement  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean remove(Object kElement) {

        for (int i = 0; i < m_iSize; i++) {

            if (kElement.equals(m_atElement[i])) {

                // element exists, shift array to fill in empty slot
                for (int j = i + 1; j < m_iSize; j++, i++) {
                    m_atElement[i] = m_atElement[j];
                }

                m_iSize--;

                return true;
            }
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int size() {
        return m_iSize;
    }


}
