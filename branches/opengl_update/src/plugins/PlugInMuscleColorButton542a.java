//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.model.structures.event.VOIEvent;
import gov.nih.mipav.model.structures.event.VOIListener;
import gov.nih.mipav.view.ColorIcon;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.JButton;

/**
 * The ColorButton inside of a ColorButtonPanel
 */
public class PlugInMuscleColorButton542a extends JButton implements VOIListener {

	/**The icon displaying the current color.*/
	private ColorIcon cIcon;
	
	/**The name of the voi corresponding to this button.*/
	private String voiName;
	
	/**Maintains reference to containing panel for easy action processing*/
	private PlugInMuscleColorButtonPanel542a container;
	
	/**
	 * Constructs a colorButton with the given color and name 
	 * with standard dimension of 20px by 20px.
	 */
	public PlugInMuscleColorButton542a(Color c, String voiName, PlugInMuscleColorButtonPanel542a container) {
		super();
		cIcon = new ColorIcon(c, 13, 13);
		this.voiName = voiName;
		this.container = container;
		
		setIcon(cIcon);
		setForeground(ColorIcon.TRANSPARENT);
		setBackground(ColorIcon.TRANSPARENT);
		setBorder(null); 
		setSize(new Dimension(20,20));
		setPreferredSize(new Dimension(20,20));
	}
	
	public PlugInMuscleColorButtonPanel542a getContainer() {
		return container;
	}
	
	/**
     * We are not interested in adding curves, so this method is empty.
     */
	public void addedCurve(VOIEvent added) {
        /* not interested in adding curves */
	}
	
    /**
	 * VOI Listener call (listens only to color changes)
	 */
	public void colorChanged(Color c) {
		cIcon.setColor(c);
		this.repaint();
	}

	/**
	 * Returns the colorIcon inside of the colorButton.
	 */
	public ColorIcon getColorIcon() {
		return cIcon;
	}
	
	/**
	 * Returns the name of the VOI corresponding to this colorButton.
	 */
	public String getVOIName() {
		return voiName;
	}

	/**
	 * We are not interested in selecting Curves, so this method is empty.
	 */
	public void selectedVOI(VOIEvent selection) {
		/* not interested in having the ColorButon select VOIs */
	}

	/**
     * We are not interested in removing Curves, so this method is empty.
     */
    public void removedCurve(VOIEvent removed) {
        /* not interested in removing curves */
    }
}