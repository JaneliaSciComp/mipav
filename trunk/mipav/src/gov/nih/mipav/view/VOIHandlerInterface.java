package gov.nih.mipav.view;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.UpdateVOISelectionListener;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIVector;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.JFrame;
import javax.swing.event.MouseInputListener;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public interface VOIHandlerInterface extends ActionListener, MouseInputListener
{
    public void addVOIUpdateListener(UpdateVOISelectionListener listener);
    public void calcPLineSliceDistances();
    public void changeVOIOrder(boolean doContour, int direction);
    public boolean checkForVOICompatibility(VOIVector VOIs, int type,
            ViewControlsImage controls);
    public boolean convertPointToPoly();
    public void copyVOIforUndo();
    public boolean copyVOItoClipBrd();
    public void deleteSelectedVOI(boolean contoursOnly);
    public void deleteVOIs();
    public void disposeLocal(boolean flag);
    public void fireVOISelectionChange(VOI voi);
    public void fireVOISelectionChange(VOI voi, VOIBase curve);
    public ModelImage getActiveImage();
    public int getActiveVOICount();
    public Point getAnchorPt();
    public Component getComponentImage();
    public JFrame getFrame();
    public float[] getImageGraphBuffer();
    public ViewJPopupPt getPopupPt();
    public ViewJPopupVOI getPopupVOI();
    public int getSlice();
    public int getVOI_ID();
    public void graph25VOI_CalcInten(boolean totalIntensity,
            boolean useThreshold, float threshold);
    public void graphPointVOI(VOI v, VOIPoint voiPt, int j,
            boolean useFrameRefTime);
    public void graphVOI();
    public boolean isLivewireNull();
    public boolean isNewVoiNeeded(int voiType);
    public void pasteVOI();
    public boolean propVOI(int direction, boolean active);
    public boolean propVOIAll();
    public void removeVOIUpdateListener(UpdateVOISelectionListener listener);
    public void resetLivewire();
    public void selectAllContours();
    public void selectAllVOIs(boolean doSelect);
    public void setCenter( Vector3f center, boolean bParent );
    public void setGraphVisible();
    public void setImageGraphBuffer(float[] buf);
    public void setMode(int mode);
    public void setModeLivewire(int selection);
    public void setPAAIGraphVisible();
    public void setVOI_ID(int ID);
    public void setVOI_IDs(int ID, int UID);
    public void setPresetHue(float presetHue);
    public void showColorDialog();
    public void showVOIProperties();
    public void updateVOIColor(Color voiColor, int voiUID);
}
