
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.awt.event.MouseEvent;
import java.io.IOException;
import java.io.Serializable;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Spatial;
import WildMagic.LibGraphics.SceneGraph.TriMesh;

public abstract class ClassificationWidget implements Serializable
{
    /**  */
    private static final long serialVersionUID = 8150358952544808439L;
    
    protected float m_fScale = .5f;
    protected Vector2f m_kTMin = new Vector2f(0,0);
    protected Vector2f m_kTMax = new Vector2f(1,1);
    protected int m_iWidth, m_iHeight;
    protected Spatial m_kPicked = null;

    protected Node m_kWidget = new Node();
    
    protected TriMesh m_kBottomOutline = null;
    protected TriMesh m_kBottomTri;
    protected ClassificationWidgetEffect m_kBottomTriEffect = null; 
    protected TriMesh m_kUpperSphere;
    protected TriMesh m_kMiddleSphere;
    protected TriMesh m_kLowerSphere;
    
    public ClassificationWidget () {}
    
    public ClassificationWidget(Vector2f kTMin, Vector2f kTMax, int iWidth, int iHeight)
    {
        m_kTMin = kTMin;
        m_kTMax = kTMax;
        m_iWidth = iWidth;
        m_iHeight = iHeight;
    }
    
    public ClassificationWidget(ClassificationWidget kWidget)
    {
        m_kTMin = new Vector2f(kWidget.m_kTMin);
        m_kTMax = new Vector2f(kWidget.m_kTMax);
        m_iWidth = kWidget.m_iWidth;
        m_iHeight = kWidget.m_iHeight;
        m_fScale = kWidget.m_fScale;
    }
    
    public void clearPicked( boolean bPicked )
    {
        for ( int i = 0; i < m_kBottomOutline.VBuffer.GetVertexQuantity(); i++ )
        {
            if ( bPicked )
            {
                m_kBottomOutline.VBuffer.SetColor3(0, i, 1f,0f,0f);
            }
            else 
            {
                m_kBottomOutline.VBuffer.SetColor3(0, i, 0f,0f,.5f);       
                m_kPicked = null;         
            }
        }
        m_kBottomOutline.VBuffer.Release();
    }
    
    public void dispose()
    {
        m_kWidget = null;
        m_kBottomOutline = null;
        m_kBottomTri = null;
        m_kBottomTriEffect = null; 
        m_kTMin = null;
        m_kTMax = null;
        m_kUpperSphere = null;
        m_kMiddleSphere = null;
        m_kLowerSphere = null;
    }
    
    public ClassificationWidgetState getState()
    {
        return m_kBottomTriEffect.getState();
    }
    
    public Node getWidget()
    {
        return m_kWidget;
    }

    public abstract boolean processMouseDrag(int iX0ld, int iYOld, int iButton, MouseEvent e );

    public void setBoundary( float fAlpha )
    {
        if ( m_kBottomTriEffect != null )
        {
            m_kBottomTriEffect.setBoundary( fAlpha );
        }
    }

    
    public ColorRGBA getColor()
    {
        ColorRGBA kColor = null;
        if ( m_kBottomTriEffect != null )
        {
            kColor = m_kBottomTriEffect.GetColor();
        }
        return kColor;
    }
    
    public void setColor( ColorRGBA kColor )
    {
        if ( m_kBottomTriEffect != null )
        {
            m_kBottomTriEffect.SetColor( kColor.R, kColor.G, kColor.B, kColor.A );
        }
    }
    
    public boolean setPicked( PickRecord kPicked )
    {
        boolean bPicked = false;
        for ( int j = 0; j < m_kWidget.GetQuantity(); j++ )
        {
            if ( kPicked.Intersected == m_kWidget.GetChild(j) )
            {
                m_kPicked = m_kWidget.GetChild(j);
                //System.err.println( m_kPicked.GetName() );
                bPicked = true;
            }
        }
        if ( bPicked )
        {
            for ( int i = 0; i < m_kBottomOutline.VBuffer.GetVertexQuantity(); i++ )
            {
                m_kBottomOutline.VBuffer.SetColor3(0, i, 1f,0f,0f);
            }
            m_kBottomOutline.VBuffer.Release();
        }
        return bPicked;
    }
    
    public void setPicked( Vector<PickRecord> kPicked )
    {
        for ( int i = 0; i < kPicked.size(); i++ )
        {
            for ( int j = 0; j < m_kWidget.GetQuantity(); j++ )
            {
                if ( kPicked.get(i).Intersected == m_kWidget.GetChild(j) )
                {
                    m_kPicked = m_kWidget.GetChild(j);
                    //System.err.println( m_kPicked.GetName() );
                }
            }
        }
        for ( int i = 0; i < m_kBottomOutline.VBuffer.GetVertexQuantity(); i++ )
        {
            m_kBottomOutline.VBuffer.SetColor3(0, i, 1f,0f,0f);
        }
        m_kBottomOutline.VBuffer.Release();
    }

    public abstract void updateDisplay();
    

    private void writeObject(java.io.ObjectOutputStream out)
    throws IOException 
    {
        out.writeObject(m_kTMin);
        out.writeObject(m_kTMax);
        out.writeInt(m_iWidth);
        out.writeInt(m_iHeight);
    }
    
    private void readObject(java.io.ObjectInputStream in)
    throws IOException, ClassNotFoundException
    {
        m_kTMin = (Vector2f)in.readObject();
        m_kTMax = (Vector2f)in.readObject();
        m_iWidth = in.readInt();
        m_iHeight = in.readInt();
    }

}
