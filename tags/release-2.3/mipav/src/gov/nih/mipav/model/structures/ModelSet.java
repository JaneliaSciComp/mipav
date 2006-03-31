package gov.nih.mipav.model.structures;


/**
*   Used in ModelSurfaceDecimator and ModelSurfaceTopology
*/

public class ModelSet
{
    
    private int m_iCapacity, m_iGrowBy, m_iSize;
    
    private Object[] m_atElement;
    
    
    public ModelSet ()
    {
        m_iCapacity = 1;
        m_iGrowBy = 1;
        m_iSize = 0;
        m_atElement = new Object[1];
    }
    
    public ModelSet (int iCapacity, int iGrowBy)
    {
        // assert:  iCapacity > 0 && iGrowBy > 0
        m_iCapacity = iCapacity;
        m_iGrowBy = iGrowBy;
        m_iSize = 0;
        m_atElement = new Object[iCapacity];
    }
    
    public ModelSet (ModelSet kSet)
    {
        // copy constructor
        m_iCapacity = kSet.m_iCapacity;
        m_iGrowBy = kSet.m_iGrowBy;
        m_iSize = kSet.m_iSize;
        m_atElement = new Object[m_iCapacity];
        System.arraycopy(kSet.m_atElement,0,m_atElement,0,m_iCapacity);
    }
    
    public int size ()
    {
        return m_iSize;
    }
    
    public Object get (int i)
    {
        if ( 0 <= i && i < m_iSize )
            return m_atElement[i];
        else
            return null;
    }
    
    public boolean isEmpty ()
    {
        return m_iSize == 0;
    }

    public boolean add (Object kElement)
    {
        for (int i = 0; i < m_iSize; i++)
        {
            if ( kElement.equals(m_atElement[i]) )
                return false;
        }

        if ( m_iSize == m_iCapacity )
        {
            // array is full, resize it
            int iNewCapacity = m_iCapacity + m_iGrowBy;
            Object[] atNewElement = new Object[iNewCapacity];
            System.arraycopy(m_atElement,0,atNewElement,0,m_iCapacity);
            m_atElement = atNewElement;
            m_iCapacity = iNewCapacity;
        }

        m_atElement[m_iSize++] = kElement;
        return true;
    }
    
    public void insertNoCheck (Object kElement)
    {
        if ( m_iSize == m_iCapacity )
        {
            // array is full, resize it
            int iNewCapacity = m_iCapacity + m_iGrowBy;
            Object[] atNewElement = new Object[iNewCapacity];
            System.arraycopy(m_atElement,0,atNewElement,0,m_iCapacity);
            m_atElement = atNewElement;
            m_iCapacity = iNewCapacity;
        }

        m_atElement[m_iSize++] = kElement;
    }
    
    public boolean remove (Object kElement)
    {
        for (int i = 0; i < m_iSize; i++)
        {
            if ( kElement.equals(m_atElement[i]) )
            {
                // element exists, shift array to fill in empty slot
                for (int j = i+1; j < m_iSize; j++, i++)
                    m_atElement[i] = m_atElement[j];

                m_iSize--;
                return true;
            }
        }

        return false;
    }
    
    public boolean exists (Object kElement)
    {
        for (int i = 0; i < m_iSize; i++)
        {
            if ( kElement.equals(m_atElement[i]) )
                return true;
        }

        return false;
    }
    
    public void clear (int iCapacity, int iGrowBy)
    {
        // assert: iCapacity > 0 && iGrowBy > 0
        m_iCapacity = iCapacity;
        m_iGrowBy = iGrowBy;
        m_iSize = 0;
        m_atElement = new Object[iCapacity];
    }
    
    
}
