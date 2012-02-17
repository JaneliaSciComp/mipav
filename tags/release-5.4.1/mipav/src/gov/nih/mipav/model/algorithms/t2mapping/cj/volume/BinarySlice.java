package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

import gov.nih.mipav.model.algorithms.t2mapping.cj.fileformats.MRIFile;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;
import java.io.*;
import java.util.*;
import java.awt.Point;

/** 
 *  The slice class that stores data for one single image.
 *
 *  Key thought here is that the data is stored in ROW-MAJOR
 *  format as that is the way most of the MRI files store 
 *  their data.  Therefore, A(row,col) is at 
 *  data[ row*ncols + col].
 */
public class BinarySlice implements Cloneable
{
	boolean[] data = null;
	private int nrows;
	private int ncols;

	final int SHIFT_ZERO = 1;
	final int SHIFT_MODULO = 2;
	final int SHIFT_COPY = 3;

	/** 
	 *  Slice constructor with nrows and cols.
	 *
	 */
	public BinarySlice(int nrows_in, int ncols_in) 
	{
		nrows = nrows_in;
		ncols = ncols_in;
		data = new boolean[nrows*ncols];
	}

	/** 
	 *  Slice constructor with nrows and cols and set all 
	 *  the locations to true as listed in the PixFile.
	 *
	 */
	public BinarySlice(int nrows_in, int ncols_in, PixFile pix) 
	{
		nrows = nrows_in;
		ncols = ncols_in;
		data = new boolean[nrows*ncols];

		/*
		 *  Now put a true everywhere the index is listed in the pix file.
		 */
		int[] pixels = pix.getIndices();
		for(int ii=0; ii<pixels.length; ii++)
		{
			data[pixels[ii]] = true;
		}
	}

	/** 
	 *  Cloner
	 *
	 */
	public Object clone()
	{
		Object o = null;
		try {
			o = super.clone();
			((BinarySlice)o).data = new boolean[ data.length ];
			System.arraycopy(data, 0, ((BinarySlice)o).data, 0, data.length);
		}
		catch( CloneNotSupportedException e) {
			System.out.println("BinarySlice can not clone");
		}

		return o;
	}

	/** 
	 *  Get the number of rows.
	 *
	 */
	final public int getNRows() 
	{ 
		return nrows; 
	}

	/** 
	 *  Get the number of columns.
	 *
	 */
	final public int getNCols() 
	{ 
		return ncols; 
	}

	final public boolean overlap(final BinarySlice other)
	{
		boolean dooverlap = false;

		if( nrows != other.nrows || ncols != other.ncols )
		{
			System.out.println("BinarySlice::overlap: Slices have different sizes");
		}
		else 
		{
			for(int ii=0; ii<nrows*ncols; ii++)
			{
				if( data[ii] && other.data[ii] ) 
				{
					dooverlap = true;
					continue;
				}
			}
		}

		return dooverlap;
	}
	
	/**
	 *  Get the data element.
	 */
	final public boolean getData(final int row, final int col)
	{
		if( !rowInBounds(row) || !colInBounds(col) ) {
			throw new ArrayIndexOutOfBoundsException("Slice::getData");
		}

		return data[row*ncols + col];
	}

	/**
	 *  Set the data element.
	 */
	final public void setData(final boolean newval, final int row, final int col)
	{
		if( !rowInBounds(row) || !colInBounds(col) ) {
			throw new ArrayIndexOutOfBoundsException("Slice::setData");
		}

		data[row*ncols + col] = newval;
	}

//	public void read(DataInput ds)
//	{
//		read( ds, MRIFile.DATATYPE_USHORT );
//	}
//
//	/** 
//	 *
//	 */
//	public void read(DataInput ds, final int datatype )
//	{
//        try {
//			if( datatype == MRIFile.DATATYPE_USHORT ) {
//				for( int ii=0; ii<nrows*ncols; ii++) {
//					data[ii] = ds.readUnsignedShort();
//				}
//			}
//			else if( datatype == MRIFile.DATATYPE_DOUBLE ) {
//				for( int ii=0; ii<nrows*ncols; ii++) {
//					data[ii] = (float)ds.readDouble();
//				}
//			}
//			else if( datatype == MRIFile.DATATYPE_FLOAT ) {
//				for( int ii=0; ii<nrows*ncols; ii++) {
//					data[ii] = ds.readFloat();
//				}
//			}
//			else if( datatype == MRIFile.DATATYPE_SHORT ) {
//				for( int ii=0; ii<nrows*ncols; ii++) {
//					data[ii] = ds.readShort();
//				}
//			}
//			else {
//				System.out.println("Slice::read: Could not figure out type.");
//				System.exit(0);
//			}
//		}
//        catch( IOException e ) {
//            System.out.println("Slice: Could not read in the MRI data.");
//            System.out.println(e);
//            System.exit(0);
//        }
//	}

	public void write(DataOutput ds, final int datatype )
	{
		if( datatype == MRIFile.DATATYPE_USHORT )  
		{
			writeUShort(ds);
		}
		else if( datatype == MRIFile.DATATYPE_SHORT ) 
		{
			writeShort(ds);
		} 
		else if( datatype == MRIFile.DATATYPE_FLOAT ) 
		{
			writeFloat(ds);
		}
		else {
            System.out.println("Volume: Could not write with data type.");
            System.exit(0);
		}
	}

	public void show()
	{
		for(int ii=0; ii<nrows; ii++) {
			for(int jj=0; jj<ncols; jj++) {
				System.out.print(data[ii*ncols+jj] + " ");
			}
			System.out.println("");
		}
	}

	private void writeFloat(DataOutput ds)
	{
        try {
			for( int ii=0; ii<nrows*ncols; ii++) {
				if( data[ii] ) ds.writeFloat((float)1.0);
				else ds.writeFloat((float)0.0);
			}
		}
        catch( IOException e ) {
            System.out.println("Volume: Could not write the MRI data.");
            System.exit(0);
        }
	}

	private void writeUShort(DataOutput ds)
	{
        try {
			for( int ii=0; ii<nrows*ncols; ii++) {
                if (data[ii]) ds.writeShort(1);
				else ds.writeShort(0);
			}
		}
        catch( IOException e ) {
            System.out.println("Volume: Could not write the MRI data.");
            System.exit(0);
        }
	}

	private void writeShort(DataOutput ds)
	{
        try {
			for( int ii=0; ii<nrows*ncols; ii++) {
                if (data[ii]) ds.writeShort(1);
				else ds.writeShort(0);
			}
		}
        catch( IOException e ) {
            System.out.println("Volume: Could not write the MRI data.");
            System.exit(0);
        }
	}

	/**
	 *  Get the row of data.
	 */
	final public boolean[] getRow(int row)
	{
		if( !rowInBounds(row)  ) {
			throw new ArrayIndexOutOfBoundsException("BinarySlice::getRow");
		}

		boolean[] tempcol = new boolean[ ncols ];
		System.arraycopy(data, ncols*row, tempcol, 0, ncols);

		return tempcol;
	}

	/**
	 *  Set the row of data.
	 */
	final public void setRow(final boolean[] newrow, final int row)
	{
		if( !rowInBounds(row) || newrow == null || newrow.length != ncols ) {
			throw new ArrayIndexOutOfBoundsException("BinarySlice::setRow");
		}

		System.arraycopy(newrow, 0, data, ncols*row, ncols);
	}

	/**
	 *  Get the column of data.
	 */
	final public boolean[] getCol(final int col)
	{
		if( !colInBounds(col)  ) {
			throw new ArrayIndexOutOfBoundsException("BinarySlice::getCol");
		}

		boolean[] tempcol = new boolean[ nrows ];
		for(int ii=0; ii<nrows; ii++) 
		{
			tempcol[ii] = data[ii*nrows + col];
		}

		return tempcol;
	}

	/**
	 *  Set the column of data.
	 */
	final public void setCol(final boolean[] tempcol, final int col)
	{
		if( !colInBounds(col) || tempcol == null || tempcol.length != nrows ) {
			throw new ArrayIndexOutOfBoundsException("BinarySlice::setCol");
		}

		for(int ii=0; ii<nrows; ii++) 
		{
			data[ii*nrows + col] = tempcol[ii];
		}
	}

	private final boolean colInBounds(final int col) 
	{
		if( col >= 0 && col < ncols ) {
			return true;
		}
		else {
			return false;
		}
	}

	private final boolean rowInBounds(final int row) {
		if( row >= 0 && row < nrows ) {
			return true;
		}
		else {
			return false;
		}
	}

	public void not( )
	{
		for(int ii=0; ii< nrows; ii++) 
		{
			for(int jj=0; jj< ncols; jj++) 
			{
				setDataUnsafe( !getDataUnsafe(ii,jj), ii, jj);
			}
		}
	}

	public void and( final BinarySlice bin )
	{
		for(int ii=0; ii< nrows; ii++) 
		{
			for(int jj=0; jj< ncols; jj++) 
			{
				setDataUnsafe( getDataUnsafe(ii,jj) && bin.getDataUnsafe(ii,jj), ii, jj);
			}
		}
	}

	public void or( final BinarySlice bin )
	{
		for(int ii=0; ii< nrows; ii++) 
		{
			for(int jj=0; jj< ncols; jj++) 
			{
				setDataUnsafe( getDataUnsafe(ii,jj) || bin.getDataUnsafe(ii,jj), ii, jj);
			}
		}
	}

	public int numberTrue( )
	{
		int total = 0;

		for(int ii=0; ii< nrows; ii++) 
		{
			for(int jj=0; jj< ncols; jj++) 
			{
				if( getDataUnsafe(ii,jj) ) total++;
			}
		}

		return total;
	}

	/*****
	 * Return a copy of the image, shifted by x pixels right and y pixels down.
	 * mode specifies the method used to fill in the pixels that fall outside
	 * of the original image:
	 *
	 * SHIFT_ZERO means fill those outer pixels with zero.
	 *
	 * SHIFT_MODULO means shift cyclically modulo the image size.
	 *
	 * SHIFT_COPY means fill in the undefined pixels with copies of their
	 *     nearest defined pixels (similar to arithmetic shift right).
	 *
	 * x and y can be negative, but must be less than the width and height
	 * of the image, regardless of shift mode.
	 *****/

	public BinarySlice shift(int x, int y, int mode)
	{
		BinarySlice out = new BinarySlice( getNRows(), getNCols() );
		int ii,jj,kk=0,ll=0;
		int M = getNRows(), N = getNCols();

//		assert(mode <= SHIFT_COPY);
//		assert(abs(x) < M);
//		assert(abs(y) < N);

		for( ii=0 ; ii<M ; ii++ )
		{
			for( jj=0 ; jj<N ; jj++ )
			{
				if ( ii-x < 0 || ii-x >= M || jj-y < 0 || jj-y >= N )
				{
					switch(mode)
					{
					case SHIFT_ZERO:
						out.setData(false, ii,jj);
						break;

					case SHIFT_MODULO:
						kk = (ii-x+M) % M;
						ll = (jj-y+N) % N;
						out.setData(getData(kk,ll), ii,jj);
						break;

					case SHIFT_COPY:
						kk = ii-x; kk = Math.max(kk,0); kk = Math.min(kk,M-1);
						ll = jj-y; ll = Math.max(ll,0); ll = Math.min(ll,N-1);
						out.setData(getData(kk,ll), ii,jj);
						break;
					}
				}
				else
				{
					out.setData(getData(kk,ll), ii-x,jj-y);
				}
			}
		}

		return out;
	}

	/*****
	 * Dilate boolean image with a given radius.  The second argument l_inf,
	 * specifies * whether to use a diagonal kernel (l_inf true) or a square
	 * kernel (l_inf false), in other words an L_1 or L_infinity norm.  
	 *
	 * Matlab's dilate(ones(3,3)) corresponds to dilate(radius=1, l_inf=true).
	 *
	 * Note that the kernel size is actually 2*abs(radius)+1, so unlike Matlab
	 * we only handle odd-sized kernels.
	 *
	 * If radius is negative, dilate uses negative logic: it dilates the 0's
	 * of the image instead of the 1's.  This is nearly identical to an erode
	 * with positive logic, except at the boundary.
	 *****/

	public void dilate(int radius, boolean l_inf)
	{
		int M = getNRows(), N = getNCols(), MN = M*N;

		int ii,jj,kk,ll,nn;
		boolean[] in = new boolean[MN];
		boolean[] out = new boolean[MN];
		boolean logic;

		if (radius==0)
		{
			return;  // radius 0 is inert.
		}
		else
		{
			logic = (radius>0);  // logic is 0 if radius is negative
			radius = Math.abs(radius);
		}

		System.arraycopy(data, 0, in, 0, MN);
		System.arraycopy(data, 0, out, 0, MN);

		/*****
		 * The quick and easy way to do this is to iterate "radius" times, each
		 * time dilating by one.  With some heavy optimisations it may be faster
		 * to do it with a sliding window, but for now this is good.
		 *
		 * I'm using straight arrays as working buffers to minimise actual
		 * accesses to the CArray object (otherwise, there can be as many as
		 * 8*radius*MN deferences, which vastly slows down this routine).
		 *****/
		for( nn=0 ; nn < radius ; nn++ )
		{
			System.arraycopy(in,0,out,0,MN);

			// For each lit pixel, turn on its neighbours
			for( ii=0 ; ii<M ; ii++ )
			{
				for( jj=0 ; jj<N ; jj++ )
				{
				   if (in[ii + M*jj] == logic)
					   {
					   // West, East, North, South neighbours
					   if (ii > 0)   out[(ii-1) + M*jj] = logic;
					   if (ii < M-1) out[(ii+1) + M*jj] = logic;
					   if (jj > 0)   out[ii + M*(jj-1)] = logic;
					   if (jj < N-1) out[ii + M*(jj+1)] = logic;

					   // If we're in L_1 norm, don't do diagonals
					   if (!l_inf) continue;

					   // Turn on NW, NE, SE, SW neighbours
					   if (ii > 0 && jj > 0)     out[(ii-1) + M*(jj-1)] = logic;
					   if (ii < M-1 && jj > 0)   out[(ii+1) + M*(jj-1)] = logic;
					   if (ii < M-1 && jj < N-1) out[(ii+1) + M*(jj+1)] = logic;
					   if (ii > 0 && jj < N-1)   out[(ii-1) + M*(jj+1)] = logic;
					   }
				}
			}
		}

		System.arraycopy(out, 0, data, 0, MN);
	}

	/** Label all connected components in the slice.
	 *  The label numbers run from 1 .. number of connected 
	 *  components and are ordered from largest connected
	 *  component to smallest.
	 */
	public Slice label()
	{
		// Create the output slice.
		Slice s = new Slice( getNRows(), getNCols() ); 

		// Set the starting label.
		int current_label = 0;

		//  Label all the points in s.
		for(int ii=0; ii<getNRows(); ii++)
		{
			for(int jj=0; jj<getNCols(); jj++)
			{
				/* If the mask is true here and the 
				 * slice is currently unlabeled, then
				 * we want to run through and label the
				 * pixels connected to (ii,jj).
				 */
				if( getData(ii,jj) && s.getData(ii,jj) == 0 )
				{
					current_label++;
					label(new Point(ii,jj), s, current_label);	
				}
			}
		}

//		//  Compute the sizes of each labelled component.
//		int[] sizes = new int [current_label+1];
//		Arrays.fill(sizes, 0);
//
//		for(int ii=0; ii<getNRows(); ii++)
//		{
//			for(int jj=0; jj<getNCols(); jj++)
//			{
//				sizes[ (int)s.getData(ii,jj) ]++;
//			}
//		}
//
		return s;
	}

	private void label(final Point p, Slice s, final int current_label)
	{
		Point currPoint = null;

		Stack stack = new Stack();
		stack.push(p);

		do
		{
			// Pop off the stack
			currPoint = (Point)stack.pop();

			// Label the component
			s.setData((double)current_label, currPoint.x, currPoint.y);

			// Push the 4 connected onto the stack
			if( currPoint.x > 0 && getData(currPoint.x-1,currPoint.y) && 
				s.getData(currPoint.x-1,currPoint.y) == 0 )
			{
				stack.push(new Point(currPoint.x-1,currPoint.y));
			}

			if( currPoint.x < (getNRows()-1) && getData(currPoint.x+1,currPoint.y) && 
				s.getData(currPoint.x+1,currPoint.y) == 0 )
			{
				stack.push(new Point(currPoint.x+1,currPoint.y));
			}

			if( currPoint.y > 0 && getData(currPoint.x,currPoint.y-1) && 
				s.getData(currPoint.x,currPoint.y-1) == 0 )
			{
				stack.push(new Point(currPoint.x,currPoint.y-1));
			}

			if( currPoint.y < (getNCols()-1) && getData(currPoint.x,currPoint.y+1) && 
				s.getData(currPoint.x,currPoint.y+1) == 0 )
			{
				stack.push(new Point(currPoint.x,currPoint.y+1));
			}

		} while( !stack.empty() );
	}

	/*****
	 * Erodes boolean image with a given radius.  The second argument l_inf,
	 * specifies * whether to use a diagonal kernel (l_inf true) or a square
	 * kernel (l_inf false), in other words an L_1 or L_infinity norm.  
	 *
	 * Matlab's erode(ones(3,3)) corresponds to erode(radius=1, l_inf=true).
	 *
	 * If radius is negative, erode uses negative logic: it erodes the 0's
	 * of the image instead of the 1's.  This is nearly identical to dilate
	 * with positive logic, except at the boundary.
	 *****/
	public void erode(int radius, boolean l_inf)
	{
		BinarySlice out = new BinarySlice(getNRows(), getNCols());
		int ii,jj,kk,ll;
		boolean logic;
		int M = getNRows(), N = getNCols();

		if ( radius==0 )
		{
			return;  // radius 0 is inert
		}
		else
		{
			logic = (radius>0);  // logic = 0 if radius is negative
		}

		dilate(-radius, l_inf);  // Erode is almost identical to dilate ;)

		radius = Math.abs(radius);

	   /*****
		* The only difference between erode and negative dilate is along the
		* boundary: erode will always black out the pixels within one radius
		* of the border.  If time critical, we should handle this within a
		* copy of the dilate loop, instead of calling dilate() and rezeroing.
		*****/

		for( ii=0 ; ii<M ; ii++ )
		{
			for( jj=0 ; jj<N ; jj++ )
			{
				if ( ii < radius || ii >= M-radius ||
					 jj < radius || jj >= N-radius )
				{
					setData(!logic, ii,jj);
				}
			}
		}
	}

final private void setDataUnsafe(boolean b,int x,int y)
	{ data[x*ncols+y] = b; }

final private boolean getDataUnsafe(int x,int y)
	{ return data[x*ncols+y]; }

	/*****
	 * Fill the holes in a binary image.  A 'hole' is defined as any maximal
	 * connected set of black pixels which does not touch any border.
	 *
	 * The diag parameter specifies whether to use 8-way connectivity (true)
	 * or 4-way connectivity (false), similar to Matlab's bwfill parameter.
	 *
	 * If logic is false, then fillHoles() uses negative logic (black is 1).
	 *****/

	public void fillHoles(boolean diag, boolean logic)
	{
		final int M = getNRows(), N = getNCols();

		int ii,jj,kk,ll;
		BinarySlice out = (BinarySlice)clone();
		PointQueue Q = new PointQueue(M*N);
		Point p = new Point(0,0);

		/*****
		 * Initialise output buffer to full; anything that isn't blackened
		 * out below belongs in the fill.  The diagonal logic is REVERSED:
		 * a 4-way foreground means we do an 8-way background fill!
		 *****/

		Arrays.fill(out.data, logic);

		/*****
		 * We do a breadth-first search fill, starting with all dark pixels
		 * along the boundary.  We'll go counterclockwise from the top-left.
		 *****/

		p.x = 0;

		// Down along left edge...The next for loop will handle (0,N-1)
		for ( p.y = 0; p.y < N-1 ; p.y++ )
		{
			if (getDataUnsafe(p.x,p.y) != logic)
			{
				out.setDataUnsafe(!logic, p.x, p.y);
				Q.push(p);
			}
		}

		// Then right along bottom edge...The next loop handles (M-1,N-1)
		for ( p.x = 0; p.x < M-1 ; p.x++ )
		{
			if (getDataUnsafe(p.x,p.y) != logic)
			{
				out.setDataUnsafe(!logic, p.x, p.y);
				Q.push(p);
			}
		}

		// Now up along right edge...The next loop handles (M-1,0)
		for ( p.y = N-1; p.y > 0 ; p.y-- )
		{
			if (getDataUnsafe(p.x,p.y) != logic)
			{
				out.setDataUnsafe(!logic, p.x, p.y);
				Q.push(p);
			}
		}

		// Last, left along top edge...(0,0) was already taken care of
		for ( p.x = M-1; p.x > 0 ; p.x-- )
		{
			if (getDataUnsafe(p.x,p.y) != logic)
			{
				out.setDataUnsafe(!logic, p.x, p.y);
				Q.push(p);
			}
		}

		/*****
		 * And now for the breadth first search...For each queue element
		 * check its neighbours.
		 *****/

		breadthFirstSearch(Q,out,diag,logic);

		System.arraycopy(out.data, 0, data, 0, out.data.length);
	}

	public void fillAt(boolean diag, boolean logic, int x, int y)
	{
		final int M = getNRows(), N = getNCols();
		BinarySlice out = new BinarySlice(M,N);
		PointQueue Q = new PointQueue(M*N);

		out.setDataUnsafe(true,x,y);

		Q.push(new Point(x,y));

		if (logic) out.not();
		breadthFirstSearch(Q, out, diag, logic);
		if (logic) out.not();

		System.arraycopy(out.data, 0, data, 0, out.data.length);
	}

// This is backwards from what is more natural.  Eventually it should
// be flipped around and fillHoles/fillAt updated.

	private final void breadthFirstSearch(PointQueue Q, BinarySlice out,
		boolean diag, boolean logic)
	{
		Point p; final int M=out.getNRows(), N=out.getNCols();

		while ( !Q.empty() )
		{
			p = Q.pop();

			// Try west neighbour
			if (p.x > 0)
			{
				p.x--;
				if (( out.getDataUnsafe(p.x,p.y) == logic ) && ( getDataUnsafe(p.x,p.y) != logic ))
				{
					out.setDataUnsafe(!logic, p.x, p.y); Q.push(p);
				}
				p.x++;
			}

			// Try east neighbour
			if (p.x < M-1)
			{
				p.x++;
				if (( out.getDataUnsafe(p.x,p.y) == logic) && ( getDataUnsafe(p.x,p.y) != logic ))
				{
					out.setDataUnsafe(!logic, p.x,p.y); Q.push(p);
				}
				p.x--;
			}

			// Try north neighbour
			if (p.y > 0)
			{
				p.y--;
				if (( out.getDataUnsafe(p.x,p.y) == logic) && ( getDataUnsafe(p.x,p.y) != logic ))
				{
					out.setDataUnsafe(!logic, p.x,p.y); Q.push(p);
				}
				p.y++;
			}

			// Try south neighbour
			if (p.y < N-1)
			{
				p.y++;
				if (( out.getDataUnsafe(p.x,p.y) == logic) && ( getDataUnsafe(p.x,p.y) != logic ))
				{
					out.setDataUnsafe(!logic, p.x,p.y); Q.push(p);
				}
				p.y--;
			}

		/*****
		 * Break out if we're not doing diagonals.  Remember,
		 * the logic of diag is reversed because we're filling
		 * the background!
		 *****/

			if (diag) continue;

			// Try north-west neighbour
			if (p.x > 0 && p.y > 0)
			{
				p.x--; p.y--;
				if (( out.getDataUnsafe(p.x,p.y) == logic) && (getDataUnsafe(p.x,p.y) != logic ))
				{
					out.setDataUnsafe(!logic,p.x,p.y); Q.push(p);
				}
				p.x++; p.y++;
			}

			// Try north-east neighbour
			if (p.x < M-1 && p.y > 0)
			{
				p.x++; p.y--;
				if (( out.getDataUnsafe(p.x,p.y) == logic) && (getDataUnsafe(p.x,p.y) != logic ))
				{
					out.setDataUnsafe(!logic,p.x,p.y); Q.push(p);
				}
				p.x--; p.y++;
			}

			// Try south-east neighbour
			if (p.x < M-1 && p.y < N-1)
			{
				p.x++; p.y++;
				if (( out.getDataUnsafe(p.x,p.y) == logic) && (getDataUnsafe(p.x,p.y) != logic ))
				{
					out.setDataUnsafe(!logic,p.x,p.y); Q.push(p);
				}
				p.x--; p.y--;
			}

			// Try south-west neighbour
			if (p.x > 0 && p.y < N-1)
			{
				p.x--; p.y++;
				if (( out.getDataUnsafe(p.x,p.y) == logic) && (getDataUnsafe(p.x,p.y) != logic ))
				{
					out.setDataUnsafe(!logic,p.x,p.y); Q.push(p);
				}
				p.x++; p.y--;
			}

		}
	}

}

class PointQueue
{
  //private AltVec qx=new AltVec(), qy=new AltVec();
  private short[] qx,qy;
  int begin=0, end=-1;
  Point p = new Point();

  PointQueue(int n) { qx = new short[n]; qy = new short[n]; }

  final public boolean empty() { return (begin > end); }
  //final public void push(Point p) { qx.add((short)p.x); qy.add((short)p.y); end++; }
  final public void push(Point p) { end++; qx[end]=(short)p.x; qy[end]=(short)p.y; }
  //final public Point pop() { p.x = qx.getShort(begin); p.y = qy.getShort(begin); begin++; return p; }
  final public Point pop() { p.x=qx[begin]; p.y=qy[begin]; begin++; return p; }
}
