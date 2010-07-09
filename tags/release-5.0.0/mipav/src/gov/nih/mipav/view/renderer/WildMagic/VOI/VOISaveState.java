package gov.nih.mipav.view.renderer.WildMagic.VOI;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.VOIVector;

public class VOISaveState
{
    public VOIVector voiVectorA;
    public VOIVector voiVectorB;
    public int currentVOI;
    public Vector3f currentCenter = new Vector3f();
    public VOISaveState() {}
}
