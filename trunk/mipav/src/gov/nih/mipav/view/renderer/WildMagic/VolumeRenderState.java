package gov.nih.mipav.view.renderer.WildMagic;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.view.renderer.WildMagic.Render.LocalVolumeVOIVector;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget;

import java.awt.Color;
import java.awt.Dimension;
import java.io.Serializable;
import java.util.Vector;

import javax.vecmath.Matrix4f;

import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

public class VolumeRenderState implements Serializable
{
    private static final long serialVersionUID = 4937933351322531830L;
    public VolumeRenderState() {};
    // Images:
    public VolumeImage ImageA;
    public VolumeImage ImageB;
    // LUT Info:
    public ModelLUT LUTa;
    public TransferFunction TransferA;
    public ModelLUT LUTb;
    public TransferFunction TransferB;
    public ModelRGB RGBa;
    public TransferFunction RedA;
    public TransferFunction GreenA;
    public TransferFunction BlueA;
    public boolean RedOnA, GreenOnA, BlueOnA;
    public ModelRGB RGBb;
    public TransferFunction RedB;
    public TransferFunction GreenB;
    public TransferFunction BlueB;
    public boolean RedOnB, GreenOnB, BlueOnB;
    // Opacity Info:
    public TransferFunction OpacityA;
    public TransferFunction OpacityGMA;
    public boolean OpacityGMOnA;
    public TransferFunction OpacityB;
    public TransferFunction OpacityGMB;
    public boolean OpacityGMOnB;
    public int SelectedTab = 0;
    
    // Menu:
    public boolean ShowAxes = true;
    public boolean ShowCrossHairs = true;
    public boolean ShowVOI = false;
    public boolean Show4D = false;
    // Position Panel:
    public boolean Radiological = true;    
    
    // Slices Panel Info
    public int[] Opacity = new int[3];
    public Color[] SliceColor = new Color[3];
    public boolean[] ShowSlice = new boolean[3];
    public boolean[] ShowSliceBox = new boolean[3];
    public Vector3f Center = new Vector3f();
    
    // Render Mode Panel Info:
    public boolean DisplayRayCast = false;
    public boolean DisplaySlices = true;
    public boolean DisplaySurface = false;
    public int StereoType = 0;
    public float IPD = 0;
    public int RenderMode = 2;
    public boolean MultiHistogram = false;
    public int VolumeBlend;
    public int ReleasedSamples;
    public int RotationSamples;
    public int ExtractionIntensityLevel = 50;
    // Custum Blend Panel Info:
    public int Equation = 0;
    public int SourceBlend = 0;
    public int DestinationBlend = 0;
    public Color BlendColor;
    public int CustumAlpha = 50;
    // MultiHistogram Panel Info:
    public Vector<ClassificationWidget> MultiHistoWidgets;
    public int WidgetSelected = -1;
    
    // Display Panel Info
    public Color BoundingBoxColor;
    public boolean ShowBoundingBox = false;
    public Color BackgroundColor;
    public boolean ShowOrientationCube = false;
    public boolean Perspective = true;
    public float[] Camera;
    public Vector3f CameraLocation = new Vector3f();
    public float[] ObjectLocation;
    public Matrix3f ObjectRotation = new Matrix3f();
    
    // Clip Panel Info
    
    // Current Tabs:
    public Vector<String> TabbedList = new Vector<String>();
    // Window Options:
    public int MainDividerLocation;
    public int PlanesDividerLocation;
    public int DualDividerLocation;
    public Dimension WindowSize;
    public int WindowX, WindowY;
    public int ExtendedState;
    
    //PlaneRender Info:
    public float[] PlaneZoom = new float[3];
    public LocalVolumeVOIVector[][] VOIList = new LocalVolumeVOIVector[3][];
    
}
