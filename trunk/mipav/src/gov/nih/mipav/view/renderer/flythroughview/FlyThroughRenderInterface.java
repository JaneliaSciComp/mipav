package gov.nih.mipav.view.renderer.flythroughview;

public interface FlyThroughRenderInterface
{

    public void makeMove(String cmd);
    public void saveAVIMovie();
    public void saveQuickTimeMovie();
    public boolean buildAnimateFrame();
    public void autoRun();
    public Object getBranchState();
    public void setCurrentState(Object _state);
    public boolean writeImage();
    public java.awt.Canvas getCanvas();
}
