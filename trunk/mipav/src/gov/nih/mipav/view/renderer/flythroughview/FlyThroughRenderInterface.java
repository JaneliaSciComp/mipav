package gov.nih.mipav.view.renderer.flythroughview;

public interface FlyThroughRenderInterface
{

    public void makeMove(String cmd);
    public boolean buildAnimateFrame();
    public void autoRun();
    public Object getBranchState();
    public void setCurrentState(Object _state);
    public boolean writeImage();
    public java.awt.Canvas getCanvas();
    public void record(boolean bOn);
    public int getCounter();
    public String getDirectory();
    public int getWidth();
    public int getHeight();
}
