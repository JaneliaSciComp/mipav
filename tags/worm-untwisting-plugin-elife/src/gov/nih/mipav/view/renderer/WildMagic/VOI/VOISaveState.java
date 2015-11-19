package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;
import WildMagic.LibFoundation.Mathematics.Vector3f;

public class VOISaveState
{
    public VOIVector voiVectorA;
    public VOIVector voiVectorB;
    public int currentVOI;
    public Vector3f currentCenter = new Vector3f();
    public VOISaveState() {}
    public void dispose() 
    { 
    	currentCenter = null;
    	if ( voiVectorA != null )
    	{
    		voiVectorA.removeAllVectorListeners();
    		for ( int i = voiVectorA.size() - 1; i >= 0; i-- )
    		{
    			VOI kVOI = voiVectorA.remove(i);
    			kVOI.dispose();
    			kVOI = null;
    		}
    		voiVectorA = null;
    	}
    	if ( voiVectorB != null )
    	{
    		voiVectorB.clear();
    		voiVectorB = null;
    	}
    }
}
