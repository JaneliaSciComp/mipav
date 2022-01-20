package gov.nih.mipav.view.renderer.J3D.surfaceview.plotterview;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;

import java.io.*;

import java.util.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Component surface created from a 2D image. The intensity values are mapped like a relief map, with higher intensities
 * making peaks and lower intensities forming valleys. A quad mesh is used to create the three-dimensional map. The same
 * LUT from the image is used to colors the vertices of the quad mesh.
 *
 * @version  0.1 Aug 1, 2001
 * @author   Matthew J. McAuliffe, Ph.D.
 * @author   Neva Cherniavsky
 * @see      ViewJFrameSurfacePlotter
 */
public class ViewJComponentSurface extends ViewJComponentBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -870321457925828704L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Vertex connection array. */
    private int[] cConnect;

    /** Vertex array. */
    private Point3f[] cVertex;

    /** Frame where the plotted surface is displayed. */
    private RenderViewBase frame;

    /** Structure holding image. */
    private ModelImage imageA;

    /** Buffer which holds the pixel data. */
    private float[] imageBufferA = null;

    /** Extents of the image. */
    private int[] imageExtents;

    /**
     * Flag indicating if the surface is currently in line mode (<code>true</code>) or fill mode (<code>false</code>).
     */
    private boolean lineMode = false;

    /** Lookup table for image. */
    private ModelLUT LUTa;

    /** Used to remap the LUT appropriately. */
    private int[] lutBufferRemapped = null;

    /** MaxBox value from the parent frame. */
    private float maxBox;

    /** Buffer used to store ARGB images of the image presently being displayed. */
    private int[] pixBuffer = null;

    /** Branch group that is the parent of the plotted surface. */
    private BranchGroup plottedRoot;

    /** Quad mesh. This is the core of the component. */
    private ModelQuadMesh qMesh;

    /** Sample size. This determines how many triangles are used in the quad mesh. */
    private int sampleSize = 1;

    /** xBox, yBox and zBox values from the parent frame. */
    private float xBox, yBox, zBox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a new component and sets up the variables.
     *
     * @param  _frame   Frame where image(s) will be displayed.
     * @param  _imageA  Model of the image that will be displayed.
     * @param  _LUTa    LUT used to display imageA.
     * @param  extents  Extents of image to be displayed.
     * @param  slice    Slice of 3D image to use, 0 if 2D image.
     */
    public ViewJComponentSurface(RenderViewBase _frame, ModelImage _imageA, ModelLUT _LUTa, int[] extents, int slice) {
        super( extents[0], extents[1], _imageA);

        frame = _frame;
        imageA = _imageA;
        imageExtents = extents;

        LUTa = _LUTa;
        lutBufferRemapped = new int[1];

        xBox = imageA.getFileInfo(0).getResolutions()[0] * extents[0];
        yBox = imageA.getFileInfo(0).getResolutions()[1] * extents[1];
        maxBox = Math.max(xBox, yBox);
        zBox = 1;
        xBox = xBox / maxBox;
        yBox = yBox / maxBox;

        setZoom(1, 1);
        setVisible(true);

        // Set up the lights for the objects
        // We need lights for this class
        setupLights();

        imageBufferA = new float[extents[0] * extents[1]];
        pixBuffer = new int[extents[0] * extents[1]];

        initSurfaceBuffers();

        try {
            imageA.exportSliceXY(slice, imageBufferA);
            calcQuadSurface(false);
        } catch (IOException error) { }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets all variables to null, disposes, and garbage collects if parameter is set.
     *
     * @param  gcFlag  If <code>true</code> garbage collector should be called.
     */
    public void dispose(boolean gcFlag) {
        lutBufferRemapped = null;
        imageBufferA = null;
        pixBuffer = null;
        frame = null;
        imageA = null;
        LUTa = null;
        imageExtents = null;
        cVertex = null;
        cConnect = null;
        imageExtents = null;

        if (plottedRoot != null) {
            Enumeration e = plottedRoot.getAllChildren();

            while (e.hasMoreElements()) {
                Object obj = e.nextElement();
                obj = null;
            }

            plottedRoot.detach();
            plottedRoot = null;
        }

        if (qMesh != null) {
            qMesh.dispose();
            qMesh = null;
        }

        super.disposeLocal();

        if (gcFlag == true) {
            System.gc();
        }
    }

    /**
     * Accessor that returns the parent frame for the component.
     *
     * @return  The parent frame.
     */
    public RenderViewBase getFrame() {
        return frame;
    }

    /**
     * Accessor that returns the image A.
     *
     * @return  imageA
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /************************************************************************/
    /**
     * Accessors.
     *
     * @return  DOCUMENT ME!
     */
    /************************************************************************/

    /**
     * Accessor that gets the model LUT for image A.
     *
     * @return  The model LUT.
     */
    public ModelLUT getLUTa() {
        return LUTa;
    }

    /**
     * Accessor that returns the quad mesh, which represents the plotted surface.
     *
     * @return  The quad mesh.
     */
    public ModelQuadMesh getQMesh() {
        return qMesh;
    }

    /**
     * Accessor that returns the sample size that created the quad mesh.
     *
     * @return  The sample size.
     */
    public int getSampleSize() {
        return sampleSize;
    }

    /**
     * Resamples and creates a new quad mesh, detaching the parent branch group and readding the surface.
     *
     * @param  sample  New sample size.
     */
    public void resample(int sample) {
        sampleSize = sample;
        plottedRoot.detach();
        plottedRoot = null;
        qMesh.dispose();
        qMesh = null;
        calcQuadSurface(false);
    }

    /**
     * Sets component's image A. Sets zoom to 1, 1.
     *
     * @param  image  Image to set to.
     */
    public void setImageA(ModelImage image) {
        imageA = image;
        setZoom(1, 1); // sets zoom
    }

    /**
     * Accessor that sets the model LUT for the image A.
     *
     * @param  LUT  The model LUT to set to.
     */
    public void setLUTa(ModelLUT LUT) {
        LUTa = LUT;
    }

    /**
     * Sets the polygon mode, if it has changed.
     *
     * @param  line  <code>true</code> indicates set to line mode, <code>false</code> indicates set to fill mode.
     */
    public void setPolygonMode(boolean line) {

        if (line != lineMode) {
            int mode;

            if (line) {
                mode = PolygonAttributes.POLYGON_LINE;
            } else {
                mode = PolygonAttributes.POLYGON_FILL;
            }

            plottedRoot.detach();

            for (Enumeration e = plottedRoot.getAllChildren(); e.hasMoreElements();) {
                Shape3D shape = (Shape3D) e.nextElement();

                shape.getAppearance().getPolygonAttributes().setPolygonMode(mode);
            }

            frame.getSceneRootTG().addChild(plottedRoot);
            lineMode = line;
        }
    }

    /**
     * Sets the quad mesh by detaching the parent branch group and readding the surface.
     *
     * @param  quadMesh  Quad mesh to set to.
     */
    public void setQMesh(ModelQuadMesh quadMesh) {
        qMesh = quadMesh;
        plottedRoot.detach();
        plottedRoot = null;
        pixBuffer = null;
        addSurface();
    }


    /**
     * sets the slice/frame # for display.
     *
     * @param  slice  slice number
     */
    public void setSlice(int slice) {

        if (imageA.getExtents().length > 2) {

            if ((slice >= 0) && (slice < imageA.getExtents()[2])) {

                try {
                    imageA.exportSliceXY(slice, imageBufferA);
                    calcQuadSurface(true);
                } catch (IOException error) {
                    error.printStackTrace();
                }
            }
        }
    }

    /**
     * Shows the surface, using the LUT to color the indices.
     *
     * @param   _LUTa        to change to new LUT for imageA else null.
     * @param   forceShow    Forces this method to import image and recalculate java image.
     * @param   _interpMode  Image interpolation method (Nearest or Smooth).
     *
     * @return  <code>true</code> indicates the show was successful.
     */
    public boolean show(ModelLUT _LUTa, boolean forceShow, int _interpMode) {

        float rangeA = 0;
        float remapConstA = 1;
        float imageMinA = 0, imageMaxA = 0;
        int xDim, yDim;
        int bufferSize;
        int lutHeightA = 0;
        int index;
        int pix;

        if (_interpMode > -1) {
            setInterpolationMode(_interpMode);
        }

        if (imageA == null) {
            return false;
        }

        if (_LUTa != null) {
            LUTa = _LUTa;
        }

        lutHeightA = LUTa.getExtents()[1];

        xDim = imageExtents[0];
        yDim = imageExtents[1];

        if (lutHeightA != lutBufferRemapped.length) {

            try {
                lutBufferRemapped = new int[lutHeightA];
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentEditImage.show");

                return false;
            }
        }

        lutBufferRemapped = LUTa.exportIndexedLUT();

        bufferSize = xDim * yDim;

        if (imageA.getType() == ModelStorageBase.UBYTE) {
            imageMinA = 0;
            imageMaxA = 255;
        } else if (imageA.getType() == ModelStorageBase.BYTE) {
            imageMinA = -128;
            imageMaxA = 127;
        } else {
            imageMinA = (float) imageA.getMin();
            imageMaxA = (float) imageA.getMax();
        }

        if (((imageMaxA - imageMinA) < 256) && (LUTa.getLUTType() == ModelLUT.STRIPED)) {
            remapConstA = 1;
        } else {
            rangeA = imageMaxA - imageMinA;

            if (rangeA == 0) {
                rangeA = 1;
            }

            if ((lutHeightA - 1) == 0) {
                remapConstA = 1;
            } else {
                remapConstA = (lutHeightA - 1) / rangeA;
            }
        }

        if ((_LUTa != null) && (qMesh != null)) {

            // qMesh.setLUT(LUTa.exportRGB_LUT(false)); This or the next should be removed.
            qMesh.setLUTBufferRemapped(lutBufferRemapped);
            qMesh.updateData((GeometryUpdater) qMesh);
        }

        for (index = 0; index < bufferSize; index++) {

            if (pixBuffer != null) {
                pix = (short) (((imageBufferA[index] - imageMinA) * remapConstA) + 0.5f);

                try {
                    pixBuffer[index] = lutBufferRemapped[pix];
                } catch (ArrayIndexOutOfBoundsException e) { }
            }
        }

        if (pixBuffer != null) {
            importImage(pixBuffer);
        }

        return true;
    }

    /**
     * Calls dispose.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        this.dispose(false);
        super.finalize();
    }

    /**
     * Sets up the surface by setting the color of the material to black, setting the polygon to no cull-face and a fill
     * mode, and allowing intersections in case we want picking ability.
     */
    private void addSurface() {
        Color4f color = new Color4f(0.0f, 0.0f, 0.0f, 0.0f);
        Material kMaterial = new Material();
        kMaterial.setCapability(Material.ALLOW_COMPONENT_READ);
        kMaterial.setCapability(Material.ALLOW_COMPONENT_WRITE);
        kMaterial.setDiffuseColor(color.x, color.y, color.z, color.w);
        kMaterial.setSpecularColor(color.x, color.y, color.z);
        kMaterial.setAmbientColor(color.x, color.y, color.z);
        // kMaterial.setShininess(0);

        // set the mesh's render state
        Appearance kAppearance = new Appearance();
        kAppearance.setCapability(Appearance.ALLOW_MATERIAL_READ);
        kAppearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);
        kAppearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_READ);
        
        kAppearance.setMaterial(kMaterial);

        // No back-face culling.  Supports double-sided meshes which can
        // regularly occur for level surfaces (open surfaces).
        PolygonAttributes kPAttr = new PolygonAttributes();
        kPAttr.setCullFace(PolygonAttributes.CULL_NONE);
        kPAttr.setCapability(PolygonAttributes.ALLOW_MODE_READ);
        kPAttr.setCapability(PolygonAttributes.ALLOW_MODE_WRITE);
        kAppearance.setPolygonAttributes(kPAttr);

        plottedRoot = new BranchGroup();
        plottedRoot.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        plottedRoot.setCapability(Group.ALLOW_CHILDREN_READ);
        plottedRoot.setCapability(Group.ALLOW_CHILDREN_WRITE);
        plottedRoot.setCapability(BranchGroup.ALLOW_DETACH);

        // For culling and picking purposes, it is better to have a separate
        // Shape3D parent for each mesh rather than a single Shape3D parent
        // for all meshes.

        Shape3D kShape = new Shape3D(qMesh, kAppearance);
        kShape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        kShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        plottedRoot.addChild(kShape);

        // Allow Java to optimize subtree.  Attach to the scene graph.
        plottedRoot.compile();
        frame.getSceneRootTG().addChild(plottedRoot);
    }

    /**
     * Calculates quad surface. Sets up indices and connections between the indices, then sets the color of the indices
     * based on the LUT.
     *
     * @param  forceUpdate  <code>true</code> updates Quad mesh, <code>false</code> not update Quad mesh.
     */
    private void calcQuadSurface(boolean forceUpdate) {

        int i, x, y;
        int offset;
        int length;
        int length1;
        int length2;
        int xDim, yDim;

        // Should not reallocate these structures each time!!! too memory expensive
        try {
            xDim = imageA.getExtents()[0];
            yDim = imageA.getExtents()[1];
            length = xDim * yDim;

            if (cVertex == null) {
                cVertex = new Point3f[length / sampleSize];
            }

            if (cConnect == null) {
                cConnect = new int[length * 4 / sampleSize];
            }
        } catch (OutOfMemoryError e) {
            System.gc();

            return;
        }

        // build connectivity
        i = 0;

        for (y = 0; y < (yDim - sampleSize); y = y + sampleSize) {
            offset = y / sampleSize * (int) Math.ceil((float) xDim / sampleSize);

            for (x = 0; x < (xDim - sampleSize); x = x + sampleSize) {
                cConnect[i++] = offset + (x / sampleSize); // 0   1
                cConnect[i++] = offset + (x / sampleSize) + 1; // 3   2
                cConnect[i++] = offset + (x / sampleSize) + (int) Math.ceil((float) xDim / sampleSize) + 1;
                cConnect[i++] = offset + (x / sampleSize) + (int) Math.ceil((float) xDim / sampleSize);
            }
        }

        length2 = i;

        float min = (float) imageA.getMin();
        float max = (float) imageA.getMax();

        float range = max - min;

        if (range == 0) {
            range = 1;
        }

        float height;
        i = 0;

        for (y = 0; y < yDim; y = y + sampleSize) {

            for (x = 0; x < xDim; x = x + sampleSize) {

                height = ((imageBufferA[(y * xDim) + x] - min) / range) - 0.5f; // Set function height relative image
                                                                                // size
                cVertex[i++] = new Point3f((x - ((xDim - 1) / 2.0f)) * (xBox / (xDim - 1)),
                                           -(y - ((yDim - 1) / 2.0f)) * (yBox / (yDim - 1)), height);
            }
        }

        length1 = i;

        try {

            if (qMesh == null) {
                qMesh = new ModelQuadMesh(cVertex, cConnect, length1, length2, LUTa.exportRGB_LUT(false), 1.0f);
                addSurface();
            } else if (forceUpdate) {
                qMesh.dispose();
                qMesh = null;
                plottedRoot.detach();
                plottedRoot = null;
                qMesh = new ModelQuadMesh(cVertex, cConnect, length1, length2, LUTa.exportRGB_LUT(false), 1.0f);
                addSurface();
            }
        } catch (OutOfMemoryError e) {
            System.gc();

            return;
        }

    }

    /**
     * Initialize the global vertext and connection arrays.
     */
    private void initSurfaceBuffers() {
        int xDim, yDim, length;

        xDim = imageA.getExtents()[0];
        yDim = imageA.getExtents()[1];

        length = xDim * yDim;

        cVertex = null;
        cConnect = null;
        System.gc();

        try {
            cVertex = new Point3f[length / sampleSize];
            cConnect = new int[length * 4 / sampleSize];
        } catch (OutOfMemoryError ofm) {
            System.out.println("Out of memory");
            System.gc();
        }
    }

    /**
     * Create and initialize the eight lights in the scene graph. The lights are positioned at the eight vertices of the
     * cube [-maxBox,maxBox]^3. Light 0 is at (-maxBox,-maxBox,-maxBox) and light 7 is at (maxBox,maxBox,maxBox). All
     * are enabled. The default color for all lights is white. The default intensity is 1.
     */
    private void setupLights() {

        // default number is 8, can be any positive value
        PointLight[] lightArray = new PointLight[8];

        for (int i = 0; i < 8; i++) {
            lightArray[i] = new PointLight();
            lightArray[i].setCapability(Light.ALLOW_COLOR_READ);
            lightArray[i].setCapability(Light.ALLOW_COLOR_WRITE);
            lightArray[i].setCapability(Light.ALLOW_STATE_READ);
            lightArray[i].setCapability(Light.ALLOW_STATE_WRITE);
            lightArray[i].setCapability(PointLight.ALLOW_POSITION_READ);
            lightArray[i].setCapability(PointLight.ALLOW_POSITION_WRITE);
            lightArray[i].setCapability(PointLight.ALLOW_ATTENUATION_READ);
            lightArray[i].setCapability(PointLight.ALLOW_ATTENUATION_WRITE);

            // default color is white
            lightArray[i].setColor(new Color3f(1.0f, 1.0f, 1.0f));

            // illuminate everything in voxel space
            lightArray[i].setInfluencingBounds(frame.bounds);

            // position at a vertex of voxel space [-maxBox,maxBox]^3
            float fX = (((i & 1) > 0) ? xBox : -xBox);
            float fY = (((i & 2) > 0) ? yBox : -yBox);
            float fZ = (((i & 4) > 0) ? zBox : -zBox);
            ((PointLight) lightArray[i]).setPosition(new Point3f(fX, fY, fZ));

            // use the constant term to control intensity
            ((PointLight) lightArray[i]).setAttenuation(2.0f, 0.0f, 0.0f);

            lightArray[i].setEnable(true);

            frame.getSceneRootTG().addChild(lightArray[i]);
        }
    }
}
