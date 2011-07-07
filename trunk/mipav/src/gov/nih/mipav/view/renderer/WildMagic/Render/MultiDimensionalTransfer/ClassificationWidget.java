
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.awt.event.MouseEvent;
import java.io.IOException;
import java.io.Serializable;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.Spatial;
import WildMagic.LibGraphics.SceneGraph.TriMesh;

public abstract class ClassificationWidget implements Serializable
{
	/**  */
	private static final long serialVersionUID = 8150358952544808439L;

	protected Vector2f m_kTMin = new Vector2f(0,0);
	protected Vector2f m_kTMax = new Vector2f(1,1);
	protected int m_iWidth, m_iHeight;
	protected Spatial m_kPicked = null;

	protected Node m_kWidget = new Node();

	protected Polyline m_kOutline = null;
	protected TriMesh m_kBottomTri;
	protected ClassificationWidgetEffect m_kBottomTriEffect = null; 
	protected TriMesh m_kUpperSphere;
	protected TriMesh m_kMiddleSphere;
	protected TriMesh m_kLowerSphere;

	protected int LEFT_EDGE = -1;
	protected int RIGHT_EDGE = 1;
	protected int BOTTOM_EDGE = -1;
	protected int TOP_EDGE = 1;
	protected float SPHERE_RADIUS = 0.04f;

	protected Vector2f m_kMouseOffset = new Vector2f();
	public ClassificationWidget () {}

	public ClassificationWidget(ClassificationWidget kWidget)
	{
		m_kTMin = new Vector2f(kWidget.m_kTMin);
		m_kTMax = new Vector2f(kWidget.m_kTMax);
		m_iWidth = kWidget.m_iWidth;
		m_iHeight = kWidget.m_iHeight;
	}

	public ClassificationWidget(Vector2f kTMin, Vector2f kTMax, int iWidth, int iHeight)
	{
		m_kTMin = kTMin;
		m_kTMax = kTMax;
		m_iWidth = iWidth;
		m_iHeight = iHeight;
	}

	public void clearPicked( )
	{    
		m_kPicked = null;         
	}

	public void clearPicked( boolean bPicked )
	{
		for ( int i = 0; i < m_kBottomTri.VBuffer.GetVertexQuantity(); i++ )
		{
			if ( bPicked )
			{
				m_kBottomTri.VBuffer.SetColor3(0, i, 1f,0f,0f);
			}
			else 
			{
				m_kBottomTri.VBuffer.SetColor3(0, i, 0f,0f,.5f);       
				m_kPicked = null;         
			}
		}
		m_kBottomTri.VBuffer.Release();
	}

	public void dispose()
	{
		m_kWidget = null;
		m_kBottomTri = null;
		m_kBottomTriEffect = null; 
		m_kTMin = null;
		m_kTMax = null;
		m_kUpperSphere = null;
		m_kMiddleSphere = null;
		m_kLowerSphere = null;
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

	public ClassificationWidgetState getState()
	{
		return m_kBottomTriEffect.getState();
	}

	public Node getWidget()
	{
		return m_kWidget;
	}

	public abstract boolean Pick( int iX, int iY );


	public boolean Pick( int iX, int iY, boolean bPicked )
	{
		float fX = calcObjX(iX);
		float fY = calcObjY(iY);
		Vector3f kCenter = m_kLowerSphere.Local.GetTranslate();
		if ( (fX > (kCenter.X - SPHERE_RADIUS)) && (fX < (kCenter.X + SPHERE_RADIUS)) &&
				(fY > (kCenter.Y - SPHERE_RADIUS)) && (fY < (kCenter.Y + SPHERE_RADIUS)) )
		{
			m_kPicked = m_kLowerSphere;
			//System.err.println( "Picked Lower" );
			bPicked = true;
		}
		kCenter = m_kUpperSphere.Local.GetTranslate();
		if ( (fX > (kCenter.X - SPHERE_RADIUS)) && (fX < (kCenter.X + SPHERE_RADIUS)) &&
				(fY > (kCenter.Y - SPHERE_RADIUS)) && (fY < (kCenter.Y + SPHERE_RADIUS)) )
		{
			m_kPicked = m_kUpperSphere;
			//System.err.println( "Picked Upper" );
			bPicked = true;
		}
		kCenter = m_kMiddleSphere.Local.GetTranslate();
		if ( (fX > (kCenter.X - SPHERE_RADIUS)) && (fX < (kCenter.X + SPHERE_RADIUS)) &&
				(fY > (kCenter.Y - SPHERE_RADIUS)) && (fY < (kCenter.Y + SPHERE_RADIUS)) )
		{
			m_kPicked = m_kMiddleSphere;
			//System.err.println( "Picked Middle" );
			bPicked = true;
		}

		if ( bPicked )
		{
			fX = 0;
			fY = 0;
			for ( int i = 0; i < m_kBottomTri.VBuffer.GetVertexQuantity(); i++ )
			{
				m_kBottomTri.VBuffer.SetColor3(0, i, 1f,0f,0f);
				fX += m_kBottomTri.VBuffer.GetPosition3fX(i);
				fY += m_kBottomTri.VBuffer.GetPosition3fY(i);
			}
			m_kBottomTri.VBuffer.Release();
			fX /= m_kBottomTri.VBuffer.GetVertexQuantity();
			fY /= m_kBottomTri.VBuffer.GetVertexQuantity();

			fX = calcScreenX(fX);
			fY = calcScreenY(fY);
			m_kMouseOffset.Set ( fX - iX, fY - iY );
		}
		return bPicked;
	}

	public abstract boolean processMouseDrag(int iX0ld, int iYOld, int iButton, MouseEvent e );

	public void setBoundary( float fAlpha )
	{
		if ( m_kBottomTriEffect != null )
		{
			m_kBottomTriEffect.setBoundary( fAlpha );
		}
	}
	
	public void setColor( ColorRGBA kColor )
	{
		if ( m_kBottomTriEffect != null )
		{
			m_kBottomTriEffect.SetColor( kColor.R, kColor.G, kColor.B, kColor.A );
		}
	}

	public abstract void updateDisplay();

	private void readObject(java.io.ObjectInputStream in)
	throws IOException, ClassNotFoundException
	{
		m_kTMin = (Vector2f)in.readObject();
		m_kTMax = (Vector2f)in.readObject();
		m_iWidth = in.readInt();
		m_iHeight = in.readInt();
	}

	private void writeObject(java.io.ObjectOutputStream out)
	throws IOException 
	{
		out.writeObject(m_kTMin);
		out.writeObject(m_kTMax);
		out.writeInt(m_iWidth);
		out.writeInt(m_iHeight);

		out.writeObject( m_kBottomTri.IBuffer );
		out.writeObject( m_kBottomTri.VBuffer );
		out.writeObject(m_kBottomTriEffect); 

		out.writeObject( m_kUpperSphere.IBuffer );
		out.writeObject( m_kUpperSphere.VBuffer );

		out.writeObject( m_kLowerSphere.IBuffer );
		out.writeObject( m_kLowerSphere.VBuffer );

		out.writeObject( m_kMiddleSphere.IBuffer );
		out.writeObject( m_kMiddleSphere.VBuffer );
		out.writeObject( m_kMiddleSphere.Local.GetTranslate() );
	}

	protected float calcObjX( float val )
	{
		float fX = val / m_iWidth;
		fX *= 2.0f;
		fX -= 1;
		return fX;
	}

	protected float calcObjY( float val )
	{
		float fY = m_iHeight - val;
		fY /= m_iHeight;
		fY *= 2.0f;
		fY -= 1;
		return fY;
	}

	protected float calcScreenX( float val )
	{
		float fX = val;
		fX += 1.0; fX /= 2.0f; fX *= m_iWidth;
		return fX;
	}

	protected float calcScreenY( float val )
	{
		float fY = val;
		fY += 1.0; fY /= 2.0f; fY *= m_iHeight; fY = m_iHeight - fY;
		return fY;
	}

	protected float calcTCoordX( float val )
	{
		float scaledVal = (val + 1)/2f;
		return (m_kTMin.X + (scaledVal) * (m_kTMax.X - m_kTMin.X));
	}

	protected float calcTCoordY( float val )
	{
		float scaledVal = (val + 1)/2f;
		return (m_kTMin.Y + (scaledVal) * (m_kTMax.Y - m_kTMin.Y));
	}

	protected Vector2f getCenter()
	{
		float fX = 0;
		float fY = 0;
		for ( int i = 0; i < m_kBottomTri.VBuffer.GetVertexQuantity(); i++ )
		{
			fX += m_kBottomTri.VBuffer.GetPosition3fX(i);
			fY += m_kBottomTri.VBuffer.GetPosition3fY(i);
		}
		fX /= m_kBottomTri.VBuffer.GetVertexQuantity();
		fY /= m_kBottomTri.VBuffer.GetVertexQuantity();
		return new Vector2f( fX, fY );
	}
}
