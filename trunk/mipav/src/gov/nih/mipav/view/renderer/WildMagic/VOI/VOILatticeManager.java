package gov.nih.mipav.view.renderer.WildMagic.VOI;

import java.awt.event.MouseEvent;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;



public class VOILatticeManager extends VOIManager
{
	VOILatticeManagerInterface m_kVOIInterface;
	
	/**
	 * Constructor. Passes in the VOIManagerInterface parent which communicates all user-interface commands to the VOIManager.
	 * @param kParent containing VOIManagerInterface.
	 */
	public VOILatticeManager (VOIManagerInterface kParent )
	{
		super(kParent);
	}

	public void setInterface( VOILatticeManagerInterface voiInterface )
	{
		m_kVOIInterface = voiInterface;
	}

	public void clear3DSelection()
	{
		if ( m_kVOIInterface != null )
		{
			m_kVOIInterface.clear3DSelection();
		}
	}

	public boolean doAutomaticLabels()
	{
		return (m_kVOIInterface == null) ? false : m_kVOIInterface.doAutomaticLabels();
	}

	public boolean is3DSelectionEnabled()
	{
		return (m_kVOIInterface == null) ? false : m_kVOIInterface.is3DSelectionEnabled();
	}

	
	public void mouseDragged(MouseEvent kEvent) {
		if ( kEvent.isControlDown() && is3DSelectionEnabled() )
		{
			if ( m_bSelected && ( m_iNearStatus == NearNone ) )
			{
				boolean bTempFirstDrag = m_bFirstDrag;
				if ( m_bFirstDrag && ((m_fMouseX != kEvent.getX()) || (m_fMouseY != kEvent.getY())) )
				{
					m_kParent.saveVOIs( "moveVOI" );
					m_bFirstDrag = false;
				} 

				Vector3f kGC = m_kDrawingContext.fileToScreenVOI( m_kCurrentVOI.getAverage() );
				Vector3f kNewGC = new Vector3f( kEvent.getX() + m_kMouseOffset.X, kEvent.getY() + m_kMouseOffset.Y, kGC.Z );
				Vector3f kDiff = Vector3f.sub( kNewGC, kGC );
				m_kParent.moveVOI( this, kDiff, m_iPlane, bTempFirstDrag, true );
				m_fMouseX = kEvent.getX();
				m_fMouseY = kEvent.getY();

		    	if ( m_kVOIInterface != null )
		    	{
		    		m_kVOIInterface.updateAnnotation( (VOIText)m_kCurrentVOI );
		    	}
			}
		}
		else
		{
			super.mouseDragged(kEvent);
		}
	}

	public void mouseMoved(MouseEvent kEvent) {
		if ( kEvent.isControlDown() && is3DSelectionEnabled() )
		{
			showSelectedVOI(kEvent);
		}
		else
		{
			super.mousePressed(kEvent);
		}
	}
	public void mousePressed(MouseEvent kEvent) {

		m_bSelected = false;
		if ( m_kCurrentVOI != null )
		{
			m_kCurrentVOI.setActive(false);
		}
		m_kCurrentVOI = null;
		m_bSelected = false;
		
		if ( kEvent.isControlDown() && is3DSelectionEnabled() )
		{
			clear3DSelection();
			if ( ( selectVOI( kEvent ) != null ) )
			{
				Vector3f kGC = m_kDrawingContext.fileToScreenVOI( m_kCurrentVOI.getAverage() );
				m_kMouseOffset.set ( kGC.X - kEvent.getX(), kGC.Y - kEvent.getY(), 0 );
				m_kParent.updateDisplay();				
			}
		}
		else
		{
			super.mousePressed(kEvent);
		}
	}

	public void mouseReleased(MouseEvent kEvent) {
		if ( kEvent.isControlDown() && is3DSelectionEnabled() )
		{
			if ( !m_bSelected )
			{
				addPoint( kEvent.getX(), kEvent.getY() );
			}
			if ( m_bSelected )
			{
				m_bSelected = false;
				if ( m_kCurrentVOI != null )
				{
					m_kCurrentVOI.setActive(false);
				}
				m_kCurrentVOI = null;
			}
			m_kParent.setCursor(MipavUtil.defaultCursor);
			m_bFirstDrag = true;
		}
		super.mouseReleased(kEvent);
	}
	
	private void addPoint( int iX, int iY ) {

		boolean picked = false;
		if ( !picked )
		{
			Vector3f kVolumePt = new Vector3f();
			if ( m_kDrawingContext.screenToFileVOI( new Vector3f (iX, iY, m_kDrawingContext.getSlice()), kVolumePt ) )
			{
				return;
			}
			
			short id = (short) m_kImageActive.getVOIs().getUniqueID();
			int colorID = 0;
			VOI newTextVOI = new VOI((short) colorID, "annotation3d_" + id, VOI.ANNOTATION, -1.0f);
			VOIText textVOI = new VOIText( );
			textVOI.add( kVolumePt );
			textVOI.add( kVolumePt );
			textVOI.setText(""+id);
			if ( m_kVOIInterface != null )
			{
				if ( doAutomaticLabels() )
				{
					textVOI.setText("" + m_kVOIInterface.getCurrentIndex() );
				}
				else
				{
					textVOI.setText("A"+ m_kVOIInterface.getCurrentIndex() );	
				}
			}
			else if ( doAutomaticLabels() )
			{
				textVOI.setText(""+id);										
			}
			else if ( !doAutomaticLabels() )
			{
				textVOI.setText("A"+id);	
			}
			newTextVOI.getCurves().add(textVOI);
			textVOI.setPlane(m_iPlane);
			add3DMarker( newTextVOI, doAutomaticLabels() );
		}
	}
	
    public void add3DMarker( VOI textVOI, boolean automaticLabel )
    {
    	if ( m_kVOIInterface != null )
    	{
    		m_kVOIInterface.add3DMarker(textVOI, automaticLabel, false);
    	}    	
    }
    
	protected VOIBase selectVOI( MouseEvent kEvent )
	{
		int iX = kEvent.getX();
		int iY = kEvent.getY();
		boolean bShiftDown = kEvent.isShiftDown();
		boolean bControlDown = false;

		m_bSelected = false;
		m_kCurrentVOI = null;
		VOIVector kVOIs = m_kImageActive.getVOIs();
		if ( kVOIs == null || kVOIs.size() <= 0 )
		{
			return null;
		}
		for ( int i = kVOIs.size()-1; i >=0; i-- )
		{
			VOI kVOI = kVOIs.get(i);
			if(kVOI.getCurves() == null) 
			{
			    continue;
			}
			for ( int j = kVOI.getCurves().size()-1; j >= 0; j-- )
			{
				VOIBase kVOI3D = kVOI.getCurves().get(j);
				if ( (kVOI3D.getType() == VOI.ANNOTATION) && (m_iPlane == (m_iPlane & kVOI3D.getPlane())) &&
						(m_kDrawingContext.getSlice() == getSlice( kVOI3D )) && 
						contains( kVOI3D, iX, iY, m_kDrawingContext.getSlice() ) )
				{
					m_kCurrentVOI = kVOI3D;
					boolean isActive = m_kCurrentVOI.isActive();
					m_kParent.setSelectedVOI( m_kCurrentVOI.getGroup(), bShiftDown, !bControlDown );
					if ( bControlDown )
					{
						m_kCurrentVOI.setActive(!isActive);
					}
					else
					{
						m_kCurrentVOI.setActive(true);                        
					}
					m_bSelected = m_kCurrentVOI.isActive();
					if ( !m_bSelected )
					{
						m_kCurrentVOI = null;
						return null;
					}
					m_kCurrentVOI.setPlane(m_iPlane);

					m_iNearStatus = NearNone;
					m_kParent.setCursor(MipavUtil.moveCursor);
					m_kParent.updateDisplay();
					return m_kCurrentVOI;
				}
			}
		}
		if ( !bControlDown )
		{
			m_kParent.selectAllVOIs(false);
		}
		return m_kCurrentVOI;
	}
	

	protected void showSelectedVOI( MouseEvent kEvent )
	{
		int iX = kEvent.getX();
		int iY = kEvent.getY(); 
		VOIVector kVOIs = m_kImageActive.getVOIs();
		if ( kVOIs == null || kVOIs.size() <= 0 )
		{
			return;
		}
		m_kComponent.removeMouseListener(m_kPopupPt);
		m_kComponent.removeMouseListener(m_kPopupVOI);
		Vector3f kVolumePt = new Vector3f();
		m_kDrawingContext.screenToFileVOI( iX, iY, m_kDrawingContext.getSlice(), kVolumePt );

		for ( int i = kVOIs.size()-1; i >=0; i-- )
		{
			VOI kVOI = kVOIs.elementAt(i);
			if(kVOI.getCurves() != null) 
			{
    			for ( int j = kVOI.getCurves().size()-1; j >= 0; j-- )
    			{
    				VOIBase kVOI3D = kVOI.getCurves().get(j);
    				if ( (kVOI3D.getType() == VOI.ANNOTATION) && (m_iPlane == (m_iPlane & kVOI3D.getPlane())) &&
    						(m_kDrawingContext.getSlice() == getSlice( kVOI3D )) && 
    						contains( kVOI3D, iX, iY, m_kDrawingContext.getSlice() ) )
    				{
    					m_iNearStatus = NearNone;
    					m_kParent.setCursor(MipavUtil.moveCursor);
    					return;
    				}
    			}
			}
		}
		m_iNearStatus = NearNone;
		m_kParent.setCursor(MipavUtil.defaultCursor);
		// not: m_kParent.setDefaultCursor() which changes the cursorMode...
	}
}
