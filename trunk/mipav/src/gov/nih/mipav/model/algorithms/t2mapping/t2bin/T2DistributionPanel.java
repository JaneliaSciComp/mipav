package gov.nih.mipav.model.algorithms.t2mapping.t2bin;

import javax.swing.*;
import javax.swing.event.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.utilities.*;


public class T2DistributionPanel extends JPanel implements ComponentListener 
{
	int width;
	int height;
	static int border = 40;

	Font font;

	boolean showLine_f;

	double max_x, max_y;

	double x[];
	double y[];

	boolean x_lower_f = false;
	double x_lim_lower = -1;

	boolean x_upper_f = false;
	double x_lim_upper = -1;

	boolean y_lower_f = false;
	double y_lim_lower = -1;

	boolean y_upper_f = false;
	double y_lim_upper = -1;

	/**
	 *
	 *  CONSTRUCTOR for the T2DistributionPanel.
	 *
	 */
	public T2DistributionPanel() {
		super();

		width = 256;
		height = 256;

		setPreferredSize(new Dimension( width, height) );

		x = null;
		y = null;

		showLine_f = true;
		font = new Font("helvetica", Font.PLAIN, 10);
	}

	/**
	 *  Set the Y data values.  Implicitly sets the X values ordinal.
	 *
	 *  @param  y_in	Double values.
	 */
	public void setY( double[] y_in ) {

		x = new double[y_in.length];
		y = new double[y_in.length];

		for(int ii=0; ii<y.length; ii++) {
			x[ii] = (double)(ii+1);
			y[ii] = y_in[ii];
		}

		for(int ii=0; ii<x.length; ii++) {
			//
			//  Find maximum y value.
			//
			max_y = Math.max( y[ii], max_y );

			//
			//  Find maximum x value.
			//
			max_x = Math.max( x[ii], max_x );
		}

		//
		//  Now repaint the window.
		//
		repaint( new Rectangle(width, height) );
	}

	/**
	 *  Set the Y data values.  Implicitly sets the X values ordinal.
	 *
	 *  @param  y_in	Integer values.
	 */
	public void setY( int[] y_in ) {

		/*
		 *   Convert the data to double then call the
		 *   double version of this function.
		 */
		double[] y_temp = new double[y_in.length];

		for(int ii=0; ii<y_in.length; ii++) {
			y_temp[ii] = (double)y_in[ii];
		}
		setY( y_temp );
	}

	/**
	 *  Set the X and Y data values.
	 *
	 *  @param  x_in	Double values.
	 *  @param  y_in	Double values.
	 */
	public void setXY( double[] x_in, double[] y_in ) {
		int ii;

		/*
		 *  Make sure they are the same length.
		 */
		if( x_in.length != y_in.length ) {
			System.out.println("The vectors are not the same length.");
			System.exit(-1);
		}

		x = new double[x_in.length];
		y = new double[y_in.length];
		for(ii=0; ii<x.length; ii++) {
			x[ii] = x_in[ii];
			y[ii] = y_in[ii];
		}

		for(ii=0; ii<x.length; ii++) {
			//
			//  Find maximum x value.
			//
			max_y = Math.max( y[ii], max_y );

			//
			//  Find maximum x value.
			//
			max_x = Math.max( x[ii], max_x );
		}

		//
		//  Now repaint the window.
		//
		repaint( new Rectangle(width, height) );
	}

	/**
	 *  Set the X and Y data values.
	 *
	 *  @param  x_in	Integer values.
	 *  @param  y_in	Integer values.
	 */
	public void setXY( int[] x_in, int[] y_in ) {

		/*
		 *  Make sure they are the same length.
		 */
		if( x_in.length != y_in.length ) {
			System.out.println("The vectors are not the same length.");
			System.exit(-1);
		}

		/*
		 *  Allocate and copy over the data.
		 */
		double[] x_temp = new double[x_in.length];
		double[] y_temp = new double[y_in.length];

		for(int ii=0; ii<x.length; ii++) {
			x_temp[ii] = (double)x_in[ii];
			y_temp[ii] = (double)y_in[ii];
		 }

		 setXY( x_temp, y_temp );
	}

	/**
	 *  Scale the X data values to fit them into the physical plot window.
	 *  This function is used only by the paint() function to 
	 *  scale the double value into a proper position in the physical
	 *  window.
	 *
	 *  @param  x_in	X location.
	 */
	private int scaleX(double x_in) {
		int x_out;

		if( x_lim_upper == -1 ) {
			/*
			 *  if we do NOT have an upper limit on the X values to show.
			 */
			x_out = (int)( x_in / max_x * (getWidth() - 2*border) );
		 }
		 else {
		 	/*
			 *  If we have an upper limit on the X values to show.
			 */
			x_out = (int)( x_in / x_lim_upper * (getWidth() - 2*border) );
		}

		x_out = x_out + border;

		return x_out;
	}

	/**
	 *  Scale the Y data values to fit them into the physical plot window.
	 *  This function is used only by the paint() function to 
	 *  scale the double value into a proper position in the physical
	 *  window.
	 *
	 *  @param  y_in	Y location.
	 */
	private int scaleY(double y_in) {
		int y_out;

		y_out = getHeight() - (int)( y_in / max_y * (getHeight() - 2*border) );

		y_out = y_out - border;

		return y_out;
	}

	/**
	 *  Show the connecting lines or not.
	 *
	 *  @param  show	Flag to show or not show.
	 */
	public void showLine( boolean show ) {
		showLine_f = show;
	}

	/**
	 *  Set the X axis lower limit.
	 *
	 *  @param  xl	Lower limit.
	 */
	public void setXLower( double xl )
	{
			x_lower_f = true;
			x_lim_lower = xl;
	}

	/**
	 *  Set the X axis upper limit.
	 *
	 *  @param  xl	Upper limit.
	 */
	public void setXUpper( double xu )
	{
			x_upper_f = true;
			x_lim_upper = xu;
	}

	/**
	 *  Paint the window.
	 *
	 *  @param  g	Graphics.
	 */
	public void paintComponent(Graphics g) {
		super.paintComponent(g);

		Format formatter = new Format("");

		//
		// Clear it.
		//
		g.clearRect(0, 0, getHeight(), getWidth());

		//
		//  x-axis
		//
		g.drawLine(border, getHeight()-border, getWidth(), getHeight()-border);

		//
		//  y-axis
		//
		g.drawLine(border, getHeight()-border, border, border);

		//
		//  Make sure there is data to plot...
		//
		if( x != null && x.length > 0 ) {
			for(int ii=0; ii<x.length; ii++) {

				if( !x_lower_f || (x_lower_f && x[ii] >= x_lim_lower ) ) {
				if( !x_upper_f || (x_upper_f && x[ii] <= x_lim_upper ) ) {
				if( !y_lower_f || (y_lower_f && y[ii] >= y_lim_lower ) ) {
				if( !y_upper_f || (y_upper_f && y[ii] <= y_lim_upper ) ) {

					//
					//  Display a point.
					//
					g.fillOval(scaleX(x[ii])-2, scaleY(y[ii])-2, 5, 5);

					//
					//  Display the line, if requested.
					//    (Why up to < x.length-1, I don't know?)
					//
					if( showLine_f && ii > 0 && ii < x.length-1) {
						g.drawLine(scaleX(x[ii-1]), scaleY(y[ii-1]),
								   scaleX(x[ii]), scaleY(y[ii]));
					}
				}}}} // bound check
			}  // if stmt

			//
			//  X Label axis tick marks and tick labels.
			//
			g.setFont(font);
			for(int ii=0; ii<x.length; ii=ii+10) {
				if( x[ii] >= x_lim_lower && x[ii] < x_lim_upper ) {
        				g.drawLine(scaleX(x[ii]), getHeight()-border-5,
	        					   scaleX(x[ii]), getHeight()-border+5);
		        		g.drawString(""+formatter.atoi(""+x[ii]), scaleX(x[ii])-(""+ii).length()/2, getHeight()-15);
				}
			}

			//
			//  The Y axis labels
			//
			for(int ii=0; ii<5; ii++) {
				g.drawLine(scaleX(-5), scaleY(y[0]+ii*(y[y.length-1]-y[0])/5), 
						scaleX(5), scaleY(y[0]+ii*(y[y.length-1]-y[0])/5)); 
				g.drawString(""+formatter.atoi(""+(y[0]+ii*(y[y.length-1]-y[0])/5)), scaleX(-85), scaleY(y[0]+ii*(y[y.length-1]-y[0])/5));
			}
		}
	}

	public void componentHidden(ComponentEvent e) {
	}

	public void componentMoved(ComponentEvent e) {
	}

	public void componentResized(ComponentEvent e) {
		Dimension curr = getParent().getPreferredSize();
		setPreferredSize( new Dimension( curr.width/2, curr.height ) );
	}

	public void componentShown(ComponentEvent e) {
	}

}
