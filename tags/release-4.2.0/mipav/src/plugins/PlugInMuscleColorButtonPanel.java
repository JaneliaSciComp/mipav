import gov.nih.mipav.model.structures.event.VOIEvent;
import gov.nih.mipav.model.structures.event.VOIListener;
import gov.nih.mipav.view.ColorIcon;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

/**
	 * simple class that contains a ColorIcon for displaying the VOI color, and 
	 * has a button for changing the color of the linked VOI (linked by name)
	 * @author linkb
	 *
	 */
	public class PlugInMuscleColorButtonPanel extends JPanel {
	
		private PlugInMuscleColorButton colorButton;
		private ActionListener container;
		
		public PlugInMuscleColorButtonPanel(Color c, String voiName, ActionListener container) {
			super();
			this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
			this.container = container;
			colorButton = new PlugInMuscleColorButton(c, voiName, this);
			colorButton.addActionListener(container);
			add(colorButton);
		}
		
		/**
		 * Returns the actual colorButton inside of the panel.
		 */
		public PlugInMuscleColorButton getColorButton() {
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
	
	/**
	 * The ColorButton inside of a ColorButtonPanel
	 */
	class PlugInMuscleColorButton extends JButton implements VOIListener {

		/**The icon displaying the current color.*/
		private ColorIcon cIcon;
		
		/**The name of the voi corresponding to this button.*/
		private String voiName;
		
		/**Maintains reference to containing panel for easy action processing*/
		private PlugInMuscleColorButtonPanel container;
		
		/**
		 * Constructs a colorButton with the given color and name 
		 * with standard dimension of 20px by 20px.
		 */
		public PlugInMuscleColorButton(Color c, String voiName, PlugInMuscleColorButtonPanel container) {
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
		
		public PlugInMuscleColorButtonPanel getContainer() {
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