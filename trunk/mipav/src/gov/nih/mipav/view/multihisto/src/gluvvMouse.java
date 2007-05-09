package gov.nih.mipav.view.multihisto.src;

public class gluvvMouse {
    //mouse state information
    public int button; //GLUT mouse button
    public int state; //GLUT mouse state
    public int[] pos = new int[2]; //pos[2], x,y
    public int[] last = new int[2]; //last[2], last mouse position
    public int shift; //is shift down
    public int ctrl; //is control down
    public int alt; //is alt down

    public gluvvMouse() {

    }
}
