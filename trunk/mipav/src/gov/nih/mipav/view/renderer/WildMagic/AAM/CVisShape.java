package gov.nih.mipav.view.renderer.WildMagic.AAM;

/**
 * INTERNAL EXTERNAL VISSHAPE
 *
 * Objects derived from <c CRect> which also contain a number of
 * bands.  These objects are used to describe parts of multi-band
 * images.
 *
 * Copyright © 1996-2000 Microsoft Corporation, All Rights Reserved
 * <nl>
 *
 * @xref <l VisShape\.inl> <c CRect>
 *
 * <nl>
 * @index | VISSHAPE
 *  
 *  CAAMshape parent class.  
 * 
 * @author Ruida Cheng
 *
 */
public class CVisShape extends CVisRect {
	
	protected int m_nbands;
	
	
	public CVisShape() { 
		super();
		m_nbands = 1;
	}

	public CVisShape(final CVisShape shape) {
		super(shape);
		m_nbands = shape.m_nbands;
	}

	public CVisShape(final RECT rect, int nbands)
	{
		super(rect);
		m_nbands = nbands;
		assert(nbands > 0);
	}

	public CVisShape(POINT point, SIZE size, int nbands) {
		super(point, size);
		m_nbands = nbands;
		assert(nbands > 0);
	}
	
	public CVisShape(int left, int top, int right, int bottom, int nbands) {
		super(left, top, right, bottom);
		m_nbands = nbands;
		assert(nbands > 0);
	}

	public int Left() {
		return left;
	}


	public int Top()
	{
		return top;
	}


	public int Right()
	{
		return right;
	}


	public int Bottom() 
	{
		return bottom;
	}

	public int NBands()
	{
		return m_nbands;
	}

	public void SetLeft(int l)
	{
		left = l;
	}

	public void SetTop(int t)
	{
		top = t;
	}

	public void SetRight(int r)
	{
		right = r;
	}

	public void SetBottom(int b)
	{
		bottom = b;
	}

	public void SetNBands(int nbands)
	{
		m_nbands = nbands;
	}
	
	
	public boolean Includes(int x, int y, int nband)
	{
		assert(left <= right);
		assert(top <= bottom);
		return (x >= left) && (x < right)
				&& (y >= top) && (y < bottom)
				&& (nband >= 0) && (nband < m_nbands);
	}

	public boolean Includes(POINT point, int nband)
	{
		assert(left <= right);
		assert(top <= bottom);
		return (point.x >= left) && (point.x < right)
				&& (point.y >= top) && (point.y < bottom)
				&& (nband >= 0) && (nband < m_nbands);
	}

	public boolean Includes(final RECT rect, int nband) 
	{
		assert(left <= right);
		assert(top <= bottom);
		assert(rect.left <= rect.right);
		assert(rect.top <= rect.bottom);
		return (rect.left >= left) && (rect.right <= right)
				&& (rect.top >= top) && (rect.bottom <= bottom)
				&& (nband >= 0) && (nband < m_nbands);
	}

	public boolean Includes(final CVisShape refshape) 
	{
		assert(left <= right);
		assert(top <= bottom);
		assert(refshape.left <= refshape.right);
		assert(refshape.top <= refshape.bottom);
		return (refshape.left >= left) && (refshape.right <= right)
				&& (refshape.top >= top) && (refshape.bottom <= bottom)
				&& (refshape.NBands() == m_nbands);
	}

	public CVisShape Decimated(int wRate, boolean expand)
	{
		CVisShape shapeT = new CVisShape(this);

		return shapeT.Decimate(wRate, expand);
	}

	public CVisShape Decimate(int wRate, boolean expand)
	{
	    if (expand) {
		    left = (int) Math.floor(left / (float) wRate);
		    top = (int) Math.floor(top / (float) wRate);
	    	right = (int) Math.ceil(right / (float) wRate);
		    bottom = (int) Math.ceil(bottom / (float) wRate);
	    } else {
		    left = (int) Math.ceil(left / (float) wRate);
		    top = (int) Math.ceil(top / (float) wRate);
		    right = 1 + (int) Math.floor((right - 1) / (float) wRate);
		    bottom = 1 + (int) Math.floor((bottom - 1) / (float) wRate);
	    }
		assert(left < right);
		assert(top < bottom);

		return this;
	}
	
	

	public CVisShape ScaledUp(int wRate) 
	{
		CVisShape shapeT = new CVisShape(this);

		return shapeT.ScaleUp(wRate);
	}

	public CVisShape ScaleUp(int wRate)
	{
		left *= wRate;
		top *= wRate;
		right *= wRate;
		bottom *= wRate;

		return this;
	}
	
	

	// CVisShape::operator==
	public boolean equals(final CVisShape shape)
	{
		assert(left <= right);
		assert(top <= bottom);
		assert(shape.left <= shape.right);
		assert(shape.top <= shape.bottom);
		return (shape.left == left) && (shape.right == right)
				&& (shape.top == top) && (shape.bottom == bottom)
				&& (shape.m_nbands == m_nbands);
	}

	public boolean notequals(final CVisShape shape)
	{
		return !this.equals(shape);
	}

	

	public CVisShape assign(final CVisShape shape)
	{
		assert(shape.left <= shape.right);
		assert(shape.top <= shape.bottom);

		left = shape.left;
		top = shape.top;
		right = shape.right;
		bottom = shape.bottom;
		m_nbands = shape.m_nbands;

		return this;
	}

	public CVisShape assign(final RECT rect)
	{
		assert(rect.left <= rect.right);
		assert(rect.top <= rect.bottom);

		left = rect.left;
		top = rect.top;
		right = rect.right;
		bottom = rect.bottom;
		m_nbands = 1;

		return this;
	}

	
	//CVisShape::operator&=
	public CVisShape bitwise_and_into(RECT rect)
	{
		assert(left <= right);
		assert(top <= bottom);
		assert(rect.left <= rect.right);
		assert(rect.top <= rect.bottom);

		if (rect.left > left)
			left = rect.left;
		if (rect.right < right)
			right = rect.right;
		if (rect.top > top)
			top = rect.top;
		if (rect.bottom < bottom)
			bottom = rect.bottom;
		m_nbands = 1;

		if ((left > right) || (top > bottom))
		{
			left = 0;
			right = 0;
			top = 0;
			bottom = 0;
		}

		return this;
	}


	// CVisShape::operator|=
	public CVisShape bitwise_or_into(final RECT rect)
	{
		assert(left <= right);
		assert(top <= bottom);
		assert(rect.left <= rect.right);
		assert(rect.top <= rect.bottom);

		if (rect.left < left)
			left = rect.left;
		if (rect.right > right)
			right = rect.right;
		if (rect.top < top)
			top = rect.top;
		if (rect.bottom > bottom)
			bottom = rect.bottom;
		m_nbands = 1;

		return this;
	}

	// CVisShape::operator&=
	public CVisShape bitwise_and_into(final CVisShape shape)
	{
		assert(left <= right);
		assert(top <= bottom);
		assert(shape.left <= shape.right);
		assert(shape.top <= shape.bottom);

		if (shape.left > left)
			left = shape.left;
		if (shape.right < right)
			right = shape.right;
		if (shape.top > top)
			top = shape.top;
		if (shape.bottom < bottom)
			bottom = shape.bottom;
		if (shape.m_nbands < m_nbands)
			m_nbands = shape.m_nbands;

		if ((left > right) || (top > bottom))
		{
			left = 0;
			right = 0;
			top = 0;
			bottom = 0;
		}

		return this;
	}

	//CVisShape::operator|=
	public CVisShape bitwise_or_into(final CVisShape shape)
	{
		assert(left <= right);
		assert(top <= bottom);
		assert(shape.left <= shape.right);
		assert(shape.top <= shape.bottom);

		if (shape.left < left)
			left = shape.left;
		if (shape.right > right)
			right = shape.right;
		if (shape.top < top)
			top = shape.top;
		if (shape.bottom > bottom)
			bottom = shape.bottom;
		if (shape.m_nbands > m_nbands)
			m_nbands = shape.m_nbands;

		return this;
	}

	//CVisShape::operator&
	public CVisShape bitwise_and(final RECT rect)
	{
		CVisShape shapeT = new CVisShape(this);
		return shapeT.bitwise_and_into(rect);
	}

	// CVisShape::operator|
	public CVisShape bitwise_or(final RECT rect)
	{
		CVisShape shapeT = new CVisShape(this);
		return shapeT.bitwise_or_into(rect);
	}

	// CVisShape::operator&
	public CVisShape bitwise_and(final CVisShape shape)
	{
		CVisShape shapeT = new CVisShape(this);
		return shapeT.bitwise_and_into(shape);
	}

	// CVisShape::operator|
	public CVisShape bitwise_or(final CVisShape shape)
	{
		CVisShape shapeT = new CVisShape(this);
		return shapeT.bitwise_or_into(shape);
	}
	

	public CVisShape IntersectedWith(final RECT rect)
	{
		CVisShape shapeT = new CVisShape(this);
		return shapeT.bitwise_and_into(rect);
	}

	public CVisShape IntersectedWith(final CVisShape shape) 
	{
		CVisShape shapeT = new CVisShape(this);
		return shapeT.bitwise_and_into(shape);
	}

	public CVisShape UnionedWith(final RECT rect)
	{
		CVisShape shapeT = new CVisShape(this);
		return shapeT.bitwise_or_into(rect);
	}

	public CVisShape UnionedWith(final CVisShape shape) 
	{
		CVisShape shapeT = new CVisShape(this);
		return shapeT.bitwise_or_into(shape);
	}

	
	
	
}