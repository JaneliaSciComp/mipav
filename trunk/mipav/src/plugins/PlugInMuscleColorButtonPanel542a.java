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

import java.awt.Color;

import java.awt.event.ActionListener;

import javax.swing.BoxLayout;

import javax.swing.JPanel;

/**
 * simple class that contains a ColorIcon for displaying the VOI color, and 
 * has a button for changing the color of the linked VOI (linked by name)
 * @author linkb
 *
 */
public class PlugInMuscleColorButtonPanel542a extends JPanel {

	private PlugInMuscleColorButton542a colorButton;
	private ActionListener container;
	
	public PlugInMuscleColorButtonPanel542a(Color c, String voiName, ActionListener container) {
		super();
		this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		this.container = container;
		colorButton = new PlugInMuscleColorButton542a(c, voiName, this);
		colorButton.addActionListener(container);
		add(colorButton);
	}
	
	/**
	 * Returns the actual colorButton inside of the panel.
	 */
	public PlugInMuscleColorButton542a getColorButton() {
		return colorButton;
	}
	
	/**
	 * Returns the listener to this panel
	 */
	public ActionListener getContainer() {
		return container;
	}

	
	
	/**
	 * Whether thecolorButton is visible.
	 */
	public boolean isSelected() {
		if(colorButton.getColorIcon().getColor() == Color.BLACK) 
			return false;
		return true;
	}

	/**
	 * Sets the color of this colorButton.
	 */
	public void setColor(Color c) {
		colorButton.getColorIcon().setColor(c);
	}
}
	
	