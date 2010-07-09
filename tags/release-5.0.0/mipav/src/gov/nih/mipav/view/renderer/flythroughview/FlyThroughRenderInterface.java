package gov.nih.mipav.view.renderer.flythroughview;

/**
 * Interface between the graphics-independent Fly-through user-interface and the
 * graphics-dependent Fly-through renderers.
 * 
 * The user-interface and graphics-independent classes for the Fly-through view
 * are in gov.nih.mipav.view.renderer.flythroughview. Both the WildMagic and J3D
 * Fly-through views work with the graphics-independent Fly-through classes by
 * implementing this interface.
 */
public interface FlyThroughRenderInterface
{
    /**
     * Passes move command from the JPanelFlythruMove to the FlyPathBehavior class.
     * @param cmd move command.
     */
    public void makeMove(String cmd);
    /**
     * @return access to the ModelImage mask image used by the Fly-through renderer.
     */
    public gov.nih.mipav.model.structures.ModelImage getImage();
    /**
     * Causes the FlyPathBehavior to fly down the current path from start to finish and back again.
     */
    public void autoRun();
    /**
     * @return the current BranchState from the FlyPatheBehavior class.
     */
    public Object getBranchState();
    /**
     * Set the BranchState in the FlyPathBehavior.
     * @param _state
     */
    public void setCurrentState(Object _state);
    /**
     * Write the current frame to disk.
     * @return true on success, false if failure to write image.
     */
    public boolean writeImage();
    /**
     * @return the Canvas from the Fly-through renderer.
     */
    public java.awt.Canvas getCanvas();
    /**
     * Turn recording frames on/off.
     * @param bOn
     */
    public void record(boolean bOn);
    /**
     * @return the current frame counter.
     */
    public int getCounter();
    /**
     * @return the frame width.
     */
    public int getWidth();
    /**
     * @return the frame height.
     */
    public int getHeight();
}
