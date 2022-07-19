package gov.nih.mipav.view.renderer;


import gov.nih.mipav.model.structures.*;
import java.awt.event.*;
import java.io.*;



/**
 * Collectiing the current virtualization toolbox values for both volume and slices render dialogs. This class is used
 * by mouse recorder to save values instantly.
 *
 * @author  Neva Cherniavsky
 * @see     JDialogBase
 */

public class SceneState implements Serializable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 9166009718509099407L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Save the current. */
    public float axisX, axisY, axisZ, axisAngle;

    /** Clipping slider values for the triplanar clipping slider x, y, z, arbitrary. */
    public int clipSliceX, clipSliceY, clipSliceZ, clipSliceA;

    /** Clipping slider values for the triplanar clipping slider x, y, z negative. */
    public int clipSliceXNeg, clipSliceYNeg, clipSliceZNeg;

    /** Boolean flag to indicate if the 6 plane clipping mode is active. */
    public boolean is6PlaneClipping = true;

    /** Boolean flag to indicate if the arbitary clipping mode is active. */
    public boolean isClipArbiPicked = false;

    /** Boolean indicator for volume opacity changed. */
    public boolean isVolOpacityChanged = false;

    /** Boolean indicator for the current diaplay mode, volume render mode or slice render mode. */
    public boolean isVolumeDisplayMode3D = false;

    /** Surface opacity slider value of the surface dialog. */
    public int surfaceOpacity;

    /** Current sceneRoot transform. */
    public float[] transform = null;

    /** Currrent tranform function of the volume render look up table. */
    public TransferFunction transformFunc;

    /** Current active volume opacity control of the volume render. */
    public int whichComp;
    // public transient ViewJComponentVolOpacityControl componentOpacityActive = null;

    /** x, y, z triplanar slider values. */
    public int x, y, z;

    /** Boolean visibility values for x, y, z, arbitrary clipping slider frames. */
    public boolean xClipVisible, yClipVisible, zClipVisible, aClipVisible;

    /** Boolean visibility values for x, y, z negative clipping slider frames. */
    public boolean xNegClipVisible, yNegClipVisible, zNegClipVisible;

    /** x, y, z triplanar opacity slider values. */
    public int xOpacity, yOpacity, zOpacity;

    /** Boolean visibility values for x, y, z triplanar. */
    public boolean xVisible, yVisible, zVisible;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct the SceneState object associate with the current visualization toobox values.
     *
     * @param  x                       triplanar x slider value.
     * @param  y                       triplanar y slider value.
     * @param  z                       triplanar z slider value.
     * @param  xOp                     triplaner x opacity slider value.
     * @param  yOp                     triplaner y opacity slider value.
     * @param  zOp                     triplaner z opacity slider value.
     * @param  xVisible                boolean for the triplanar x slider frame visibility value.
     * @param  yVisible                boolean for the triplanar y slider frame visibility value.
     * @param  zVisible                boolean for the triplanar z slider frame visibility value.
     * @param  surfaceOpacity          surface opacity slider value for the surface dialog.
     * @param  clipA                   clip slider arbitrary value.
     * @param  clipX                   clip slider x value.
     * @param  clipY                   clip slider y value.
     * @param  clipZ                   clip slider z value.
     * @param  clipXNeg                clip slider x negative value.
     * @param  clipYNeg                clip slider y negative value.
     * @param  clipZNeg                clip slider z negative value.
     * @param  aClipV                  clip slider arbitrary visibility value.
     * @param  xClipV                  clip slider x boolean visibility value.
     * @param  yClipV                  clip slider y boolean visibility value.
     * @param  zClipV                  clip slider z boolean visibility value.
     * @param  xNegClipV               clip slider x negative boolean visiblity value.
     * @param  yNegClipV               clip slider y negative boolean visiblity value.
     * @param  zNegClipV               clip slider z negative boolean visiblity value.
     * @param  _whichComp              volume control current active component.
     * @param  _transformFunc          volume control instant tranfer function.
     * @param  _isVolOpacityChanged    boolean indicator for volume control opacity changed.
     * @param  _isVolumeDisplayMode3D  current diaplay mode, volume render or slice render.
     * @param  _is6PlaneClipping       If clipping is 6-plane clipped, choose <tt>true</tt>.
     * @param  _axisX                  Location of the X-axis?
     * @param  _axisY                  Location of the Y-axis?
     * @param  _axisZ                  Location of the Z-axis?
     * @param  _axisAngle              Angle the clipping plane makes with the X-Y plane?
     * @param  _isClipArbiPicked       If clipping is arbitrarily picked, choose <tt>true</tt>.
     */
    public SceneState(int x, int y, int z, int xOp, int yOp, int zOp, boolean xVisible, boolean yVisible,
                      boolean zVisible, int surfaceOpacity, int clipA, int clipX, int clipY, int clipZ, int clipXNeg,
                      int clipYNeg, int clipZNeg, boolean aClipV, boolean xClipV, boolean yClipV, boolean zClipV,
                      boolean xNegClipV, boolean yNegClipV, boolean zNegClipV, int _whichComp,
                      TransferFunction _transformFunc, boolean _isVolOpacityChanged, boolean _isVolumeDisplayMode3D,
                      boolean _is6PlaneClipping, float _axisX, float _axisY, float _axisZ, float _axisAngle,
                      boolean _isClipArbiPicked) {
        int i, transFuncLength = 0;
        this.x = x;
        this.y = y;
        this.z = z;
        this.xOpacity = xOp;
        this.yOpacity = yOp;
        this.zOpacity = zOp;
        this.xVisible = xVisible;
        this.yVisible = yVisible;
        this.zVisible = zVisible;
        this.surfaceOpacity = surfaceOpacity;
        this.clipSliceA = clipA;
        this.clipSliceX = clipX;
        this.clipSliceY = clipY;
        this.clipSliceZ = clipZ;
        this.clipSliceXNeg = clipXNeg;
        this.clipSliceYNeg = clipYNeg;
        this.clipSliceZNeg = clipZNeg;
        this.aClipVisible = aClipV;
        this.xClipVisible = xClipV;
        this.yClipVisible = yClipV;
        this.zClipVisible = zClipV;
        this.xNegClipVisible = xNegClipV;
        this.yNegClipVisible = yNegClipV;
        this.zNegClipVisible = zNegClipV;
        this.whichComp = _whichComp;

        if (_transformFunc != null) {

            for (i = 0; i < _transformFunc.size(); i++) {

                if (_transformFunc.getPoint(i) != null) {
                    transFuncLength++;
                } else {
                    break;
                }
            }

            this.transformFunc = new TransferFunction();

            for (i = 0; i < transFuncLength; i++) {
                this.transformFunc.addPoint(_transformFunc.getPoint(i).X, _transformFunc.getPoint(i).Y);
            }
        }

        this.isVolOpacityChanged = _isVolOpacityChanged;
        this.isVolumeDisplayMode3D = _isVolumeDisplayMode3D;
        this.is6PlaneClipping = _is6PlaneClipping;
        this.axisX = _axisX;
        this.axisY = _axisY;
        this.axisZ = _axisZ;
        this.axisAngle = _axisAngle;
        this.isClipArbiPicked = _isClipArbiPicked;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Stub method for classes that extend this Object.
     *
     * @param  event  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent event) { } /* stub */

    /**
     * Print out the present screen state.
     *
     * @return  DOCUMENT ME!
     */
    public String toString() {
        return ("SceneState[" + x + "," + y + "," + z + "," + xOpacity + "," + yOpacity + "," + zOpacity + "," +
                xVisible + "," + yVisible + "," + zVisible + "," + surfaceOpacity + "," + clipSliceA + "," +
                clipSliceX + "," + clipSliceY + "," + clipSliceZ + "," + clipSliceXNeg + "," + clipSliceYNeg + "," +
                clipSliceZNeg + "," + aClipVisible + "," + xClipVisible + "," + yClipVisible + "," + zClipVisible +
                "," + xNegClipVisible + "," + yNegClipVisible + "," + zNegClipVisible + "," + whichComp + "," +
                transformFunc + "," + isVolOpacityChanged + "," + isVolumeDisplayMode3D + "," + is6PlaneClipping + "," +
                axisX + "," + axisY + "," + axisZ + "," + axisAngle + "," + isClipArbiPicked + "]");
    }
}
