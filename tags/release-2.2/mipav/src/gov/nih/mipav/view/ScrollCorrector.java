package gov.nih.mipav.view;

import javax.swing.*;
import java.awt.event.*;

/** Corrects a strange behaviour with JScrollPane when lines are 
*   appended from another thread.  Moves the vertical slider to 
*   the last position.
*   <p>
*   usage: add as an adjustment listener to the VerticalScrollBar, 
*   eg.,<code>
*   scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());
*   </code>
*   <p>
*   Taken as a modified version from 
*   traiton , on Sun's bug parade.  See:
*   <a href="http://developer.java.sun.com/developer/bugParade/bugs/4201999.html">JTextArea's don't 
*   automatically scoll when appending() to them</a>.  20 February 2001 entry.
*   <p>
*   Modified as needed.
*   @author traiton
*   
*/
public class ScrollCorrector implements AdjustmentListener {
    private int     lastMax = 0;
    private boolean atLastPos = true;
    
    /** Does nothing */
    public ScrollCorrector() {}
    
    /**
    */
    public void adjustmentValueChanged(AdjustmentEvent ae) {
        //System.out.println(">"+ae.paramString()+"<");
        JScrollBar vscroll = (JScrollBar) ae.getAdjustable();
        BoundedRangeModel brm = vscroll.getModel();
        int maxValue = brm.getMaximum();
        if (vscroll.getValueIsAdjusting()) {
            int curValue = brm.getValue();
            int extent = brm.getExtent();
            int diff = maxValue - curValue - extent;
            if (diff == 0) {
                atLastPos = true;
            }
            else {
                atLastPos = false;
            }
        }
        else {
            // maxValue > lastMax means that a new line has been appended.
            if (maxValue > lastMax && atLastPos) {
                brm.setValue(maxValue);
                lastMax = maxValue;
            }
        }
    }
}
