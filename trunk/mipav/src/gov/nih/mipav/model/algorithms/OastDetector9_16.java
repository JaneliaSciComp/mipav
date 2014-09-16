package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.Vector;

import javax.vecmath.Point2d;
import javax.vecmath.Point2i;

/**
oast9 - OAST, an optimal corner detector based on the
        accelerated segment test for a 16 pixel mask

Copyright (c) 2010, Elmar Mair
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
   * Neither the name of the owner nor the names of its contributors may be 
     used to endorse or promote products derived from this software without 
     specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


public class OastDetector9_16 {
	
	
	private int xsize = 0;
	
	private int ysize = 0;
	
	private int threshold = -1;
	
	private int  s_offset0;
	private int  s_offset1;
	private int  s_offset2;
	private int  s_offset3;
	private int  s_offset4;
	private int  s_offset5;
	private int  s_offset6;
	private int  s_offset7;
	private int  s_offset8;
	private int  s_offset9;
	private int  s_offset10;
	private int  s_offset11;
	private int  s_offset12;
	private int  s_offset13;
	private int  s_offset14;
	private int  s_offset15;

	public OastDetector9_16(int xsize, int ysize, int threshold) {
	    this.xsize = xsize;
	    this.ysize = ysize;
	    this.threshold = threshold;
	    init_pattern();
	}
	
	private void init_pattern()
	{
		s_offset0=(-3)+(0)*xsize;
		s_offset1=(-3)+(-1)*xsize;
		s_offset2=(-2)+(-2)*xsize;
		s_offset3=(-1)+(-3)*xsize;
		s_offset4=(0)+(-3)*xsize;
		s_offset5=(1)+(-3)*xsize;
		s_offset6=(2)+(-2)*xsize;
		s_offset7=(3)+(-1)*xsize;
		s_offset8=(3)+(0)*xsize;
		s_offset9=(3)+(1)*xsize;
		s_offset10=(2)+(2)*xsize;
		s_offset11=(1)+(3)*xsize;
		s_offset12=(0)+(3)*xsize;
		s_offset13=(-1)+(3)*xsize;
		s_offset14=(-2)+(2)*xsize;
		s_offset15=(-3)+(1)*xsize;
	}
	
	public void setThreshold(int threshold) {
		this.threshold = threshold;
	}
	
	public void detect(int intBuffer[], Vector<Point2i> corners_all)
	{
		int total=0;
		int nExpectedCorners=corners_all.capacity();
		Point2i h = new Point2i();
		int x, y;
		int xsizeB=xsize - 4;
		int ysizeB=ysize - 3;
	    int  offset0, offset1, offset2, offset3, offset4, offset5, offset6, offset7, offset8,
	         offset9, offset10, offset11, offset12, offset13, offset14, offset15;

		corners_all.clear();

	    offset0=s_offset0;
	    offset1=s_offset1;
	    offset2=s_offset2;
	    offset3=s_offset3;
	    offset4=s_offset4;
	    offset5=s_offset5;
	    offset6=s_offset6;
	    offset7=s_offset7;
	    offset8=s_offset8;
	    offset9=s_offset9;
	    offset10=s_offset10;
	    offset11=s_offset11;
	    offset12=s_offset12;
	    offset13=s_offset13;
	    offset14=s_offset14;
	    offset15=s_offset15;

		for(y=3; y < ysizeB; y++)			
		{										
			x=2;								
			while(true)							
			{									
				x++;			
				if(x>xsizeB)	
					break;
				else
				{
					int p = y*xsize + x;
					int cb = intBuffer[p] + threshold;
					int c_b = intBuffer[p] - threshold;
					if(intBuffer[p + offset0] > cb)
					  if(intBuffer[p + offset2] > cb)
						if(intBuffer[p + offset4] > cb)
						  if(intBuffer[p + offset5] > cb)
							if(intBuffer[p + offset7] > cb)
							  if(intBuffer[p + offset3] > cb)
								if(intBuffer[p + offset1] > cb)
								  if(intBuffer[p + offset6] > cb)
									if(intBuffer[p + offset8] > cb)
									  {}
									else
									  if(intBuffer[p + offset15] > cb)
										{}
									  else
										continue;
								  else
									if(intBuffer[p + offset13] > cb)
									  if(intBuffer[p + offset14] > cb)
										if(intBuffer[p + offset15] > cb)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset8] > cb)
									if(intBuffer[p + offset9] > cb)
									  if(intBuffer[p + offset10] > cb)
										if(intBuffer[p + offset6] > cb)
										  {}
										else
										  if(intBuffer[p + offset11] > cb)
											if(intBuffer[p + offset12] > cb)
											  if(intBuffer[p + offset13] > cb)
												if(intBuffer[p + offset14] > cb)
												  if(intBuffer[p + offset15] > cb)
													{}
												  else
													continue;
												else
												  continue;
											  else
												continue;
											else
											  continue;
										  else
											continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								if(intBuffer[p + offset10] > cb)
								  if(intBuffer[p + offset11] > cb)
									if(intBuffer[p + offset12] > cb)
									  if(intBuffer[p + offset8] > cb)
										if(intBuffer[p + offset9] > cb)
										  if(intBuffer[p + offset6] > cb)
											{}
										  else
											if(intBuffer[p + offset13] > cb)
											  if(intBuffer[p + offset14] > cb)
												if(intBuffer[p + offset15] > cb)
												  {}
												else
												  continue;
											  else
												continue;
											else
											  continue;
										else
										  if(intBuffer[p + offset1] > cb)
											if(intBuffer[p + offset13] > cb)
											  if(intBuffer[p + offset14] > cb)
												if(intBuffer[p + offset15] > cb)
												  {}
												else
												  continue;
											  else
												continue;
											else
											  continue;
										  else
											continue;
									  else
										if(intBuffer[p + offset1] > cb)
										  if(intBuffer[p + offset13] > cb)
											if(intBuffer[p + offset14] > cb)
											  if(intBuffer[p + offset15] > cb)
												{}
											  else
												continue;
											else
											  continue;
										  else
											continue;
										else
										  continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							else if(intBuffer[p + offset7] < c_b)
							  if(intBuffer[p + offset14] > cb)
								if(intBuffer[p + offset15] > cb)
								  if(intBuffer[p + offset1] > cb)
									if(intBuffer[p + offset3] > cb)
									  if(intBuffer[p + offset6] > cb)
										{}
									  else
										if(intBuffer[p + offset13] > cb)
										  {}
										else
										  continue;
									else
									  if(intBuffer[p + offset10] > cb)
										if(intBuffer[p + offset11] > cb)
										  if(intBuffer[p + offset12] > cb)
											if(intBuffer[p + offset13] > cb)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset8] > cb)
									  if(intBuffer[p + offset9] > cb)
										if(intBuffer[p + offset10] > cb)
										  if(intBuffer[p + offset11] > cb)
											if(intBuffer[p + offset12] > cb)
											  if(intBuffer[p + offset13] > cb)
												{}
											  else
												continue;
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  continue;
							  else if(intBuffer[p + offset14] < c_b)
								if(intBuffer[p + offset8] < c_b)
								  if(intBuffer[p + offset9] < c_b)
									if(intBuffer[p + offset10] < c_b)
									  if(intBuffer[p + offset11] < c_b)
										if(intBuffer[p + offset12] < c_b)
										  if(intBuffer[p + offset13] < c_b)
											if(intBuffer[p + offset6] < c_b)
											  {}
											else
											  if(intBuffer[p + offset15] < c_b)
												{}
											  else
												continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							  else
								continue;
							else
							  if(intBuffer[p + offset14] > cb)
								if(intBuffer[p + offset15] > cb)
								  if(intBuffer[p + offset1] > cb)
									if(intBuffer[p + offset3] > cb)
									  if(intBuffer[p + offset6] > cb)
										{}
									  else
										if(intBuffer[p + offset13] > cb)
										  {}
										else
										  continue;
									else
									  if(intBuffer[p + offset10] > cb)
										if(intBuffer[p + offset11] > cb)
										  if(intBuffer[p + offset12] > cb)
											if(intBuffer[p + offset13] > cb)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset8] > cb)
									  if(intBuffer[p + offset9] > cb)
										if(intBuffer[p + offset10] > cb)
										  if(intBuffer[p + offset11] > cb)
											if(intBuffer[p + offset12] > cb)
											  if(intBuffer[p + offset13] > cb)
												{}
											  else
												continue;
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  continue;
							  else
								continue;
						  else if(intBuffer[p + offset5] < c_b)
							if(intBuffer[p + offset12] > cb)
							  if(intBuffer[p + offset13] > cb)
								if(intBuffer[p + offset14] > cb)
								  if(intBuffer[p + offset15] > cb)
									if(intBuffer[p + offset1] > cb)
									  if(intBuffer[p + offset3] > cb)
										{}
									  else
										if(intBuffer[p + offset10] > cb)
										  if(intBuffer[p + offset11] > cb)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset8] > cb)
										if(intBuffer[p + offset9] > cb)
										  if(intBuffer[p + offset10] > cb)
											if(intBuffer[p + offset11] > cb)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset6] > cb)
									  if(intBuffer[p + offset7] > cb)
										if(intBuffer[p + offset8] > cb)
										  if(intBuffer[p + offset9] > cb)
											if(intBuffer[p + offset10] > cb)
											  if(intBuffer[p + offset11] > cb)
												{}
											  else
												continue;
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  continue;
							  else
								continue;
							else if(intBuffer[p + offset12] < c_b)
							  if(intBuffer[p + offset7] < c_b)
								if(intBuffer[p + offset8] < c_b)
								  if(intBuffer[p + offset9] < c_b)
									if(intBuffer[p + offset10] < c_b)
									  if(intBuffer[p + offset11] < c_b)
										if(intBuffer[p + offset13] < c_b)
										  if(intBuffer[p + offset6] < c_b)
											{}
										  else
											if(intBuffer[p + offset14] < c_b)
											  if(intBuffer[p + offset15] < c_b)
												{}
											  else
												continue;
											else
											  continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							  else
								continue;
							else
							  continue;
						  else
							if(intBuffer[p + offset12] > cb)
							  if(intBuffer[p + offset13] > cb)
								if(intBuffer[p + offset14] > cb)
								  if(intBuffer[p + offset15] > cb)
									if(intBuffer[p + offset1] > cb)
									  if(intBuffer[p + offset3] > cb)
										{}
									  else
										if(intBuffer[p + offset10] > cb)
										  if(intBuffer[p + offset11] > cb)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset8] > cb)
										if(intBuffer[p + offset9] > cb)
										  if(intBuffer[p + offset10] > cb)
											if(intBuffer[p + offset11] > cb)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset6] > cb)
									  if(intBuffer[p + offset7] > cb)
										if(intBuffer[p + offset8] > cb)
										  if(intBuffer[p + offset9] > cb)
											if(intBuffer[p + offset10] > cb)
											  if(intBuffer[p + offset11] > cb)
												{}
											  else
												continue;
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  continue;
							  else
								continue;
							else if(intBuffer[p + offset12] < c_b)
							  if(intBuffer[p + offset7] < c_b)
								if(intBuffer[p + offset8] < c_b)
								  if(intBuffer[p + offset9] < c_b)
									if(intBuffer[p + offset10] < c_b)
									  if(intBuffer[p + offset11] < c_b)
										if(intBuffer[p + offset13] < c_b)
										  if(intBuffer[p + offset14] < c_b)
											if(intBuffer[p + offset6] < c_b)
											  {}
											else
											  if(intBuffer[p + offset15] < c_b)
												{}
											  else
												continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							  else
								continue;
							else
							  continue;
						else if(intBuffer[p + offset4] < c_b)
						  if(intBuffer[p + offset11] > cb)
							if(intBuffer[p + offset12] > cb)
							  if(intBuffer[p + offset13] > cb)
								if(intBuffer[p + offset10] > cb)
								  if(intBuffer[p + offset14] > cb)
									if(intBuffer[p + offset15] > cb)
									  if(intBuffer[p + offset1] > cb)
										{}
									  else
										if(intBuffer[p + offset8] > cb)
										  if(intBuffer[p + offset9] > cb)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset6] > cb)
										if(intBuffer[p + offset7] > cb)
										  if(intBuffer[p + offset8] > cb)
											if(intBuffer[p + offset9] > cb)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset5] > cb)
									  if(intBuffer[p + offset6] > cb)
										if(intBuffer[p + offset7] > cb)
										  if(intBuffer[p + offset8] > cb)
											if(intBuffer[p + offset9] > cb)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset1] > cb)
									if(intBuffer[p + offset3] > cb)
									  if(intBuffer[p + offset14] > cb)
										if(intBuffer[p + offset15] > cb)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								continue;
							else
							  continue;
						  else if(intBuffer[p + offset11] < c_b)
							if(intBuffer[p + offset7] < c_b)
							  if(intBuffer[p + offset8] < c_b)
								if(intBuffer[p + offset9] < c_b)
								  if(intBuffer[p + offset10] < c_b)
									if(intBuffer[p + offset6] < c_b)
									  if(intBuffer[p + offset5] < c_b)
										if(intBuffer[p + offset3] < c_b)
										  {}
										else
										  if(intBuffer[p + offset12] < c_b)
											{}
										  else
											continue;
									  else
										if(intBuffer[p + offset12] < c_b)
										  if(intBuffer[p + offset13] < c_b)
											if(intBuffer[p + offset14] < c_b)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset12] < c_b)
										if(intBuffer[p + offset13] < c_b)
										  if(intBuffer[p + offset14] < c_b)
											if(intBuffer[p + offset15] < c_b)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									continue;
								else
								  continue;
							  else
								continue;
							else
							  continue;
						  else
							continue;
						else
						  if(intBuffer[p + offset11] > cb)
							if(intBuffer[p + offset12] > cb)
							  if(intBuffer[p + offset13] > cb)
								if(intBuffer[p + offset10] > cb)
								  if(intBuffer[p + offset14] > cb)
									if(intBuffer[p + offset15] > cb)
									  if(intBuffer[p + offset1] > cb)
										{}
									  else
										if(intBuffer[p + offset8] > cb)
										  if(intBuffer[p + offset9] > cb)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset6] > cb)
										if(intBuffer[p + offset7] > cb)
										  if(intBuffer[p + offset8] > cb)
											if(intBuffer[p + offset9] > cb)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset5] > cb)
									  if(intBuffer[p + offset6] > cb)
										if(intBuffer[p + offset7] > cb)
										  if(intBuffer[p + offset8] > cb)
											if(intBuffer[p + offset9] > cb)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset1] > cb)
									if(intBuffer[p + offset3] > cb)
									  if(intBuffer[p + offset14] > cb)
										if(intBuffer[p + offset15] > cb)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								continue;
							else
							  continue;
						  else if(intBuffer[p + offset11] < c_b)
							if(intBuffer[p + offset7] < c_b)
							  if(intBuffer[p + offset8] < c_b)
								if(intBuffer[p + offset9] < c_b)
								  if(intBuffer[p + offset10] < c_b)
									if(intBuffer[p + offset12] < c_b)
									  if(intBuffer[p + offset13] < c_b)
										if(intBuffer[p + offset6] < c_b)
										  if(intBuffer[p + offset5] < c_b)
											{}
										  else
											if(intBuffer[p + offset14] < c_b)
											  {}
											else
											  continue;
										else
										  if(intBuffer[p + offset14] < c_b)
											if(intBuffer[p + offset15] < c_b)
											  {}
											else
											  continue;
										  else
											continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							  else
								continue;
							else
							  continue;
						  else
							continue;
					  else if(intBuffer[p + offset2] < c_b)
						if(intBuffer[p + offset9] > cb)
						  if(intBuffer[p + offset10] > cb)
							if(intBuffer[p + offset11] > cb)
							  if(intBuffer[p + offset8] > cb)
								if(intBuffer[p + offset12] > cb)
								  if(intBuffer[p + offset13] > cb)
									if(intBuffer[p + offset14] > cb)
									  if(intBuffer[p + offset15] > cb)
										{}
									  else
										if(intBuffer[p + offset6] > cb)
										  if(intBuffer[p + offset7] > cb)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset5] > cb)
										if(intBuffer[p + offset6] > cb)
										  if(intBuffer[p + offset7] > cb)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset4] > cb)
									  if(intBuffer[p + offset5] > cb)
										if(intBuffer[p + offset6] > cb)
										  if(intBuffer[p + offset7] > cb)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset3] > cb)
									if(intBuffer[p + offset4] > cb)
									  if(intBuffer[p + offset5] > cb)
										if(intBuffer[p + offset6] > cb)
										  if(intBuffer[p + offset7] > cb)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								if(intBuffer[p + offset1] > cb)
								  if(intBuffer[p + offset12] > cb)
									if(intBuffer[p + offset13] > cb)
									  if(intBuffer[p + offset14] > cb)
										if(intBuffer[p + offset15] > cb)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							else
							  continue;
						  else
							continue;
						else if(intBuffer[p + offset9] < c_b)
						  if(intBuffer[p + offset7] < c_b)
							if(intBuffer[p + offset8] < c_b)
							  if(intBuffer[p + offset6] < c_b)
								if(intBuffer[p + offset5] < c_b)
								  if(intBuffer[p + offset4] < c_b)
									if(intBuffer[p + offset3] < c_b)
									  if(intBuffer[p + offset1] < c_b)
										{}
									  else
										if(intBuffer[p + offset10] < c_b)
										  {}
										else
										  continue;
									else
									  if(intBuffer[p + offset10] < c_b)
										if(intBuffer[p + offset11] < c_b)
										  if(intBuffer[p + offset12] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset10] < c_b)
									  if(intBuffer[p + offset11] < c_b)
										if(intBuffer[p + offset12] < c_b)
										  if(intBuffer[p + offset13] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset10] < c_b)
									if(intBuffer[p + offset11] < c_b)
									  if(intBuffer[p + offset12] < c_b)
										if(intBuffer[p + offset13] < c_b)
										  if(intBuffer[p + offset14] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								if(intBuffer[p + offset10] < c_b)
								  if(intBuffer[p + offset11] < c_b)
									if(intBuffer[p + offset12] < c_b)
									  if(intBuffer[p + offset13] < c_b)
										if(intBuffer[p + offset14] < c_b)
										  if(intBuffer[p + offset15] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							else
							  continue;
						  else
							continue;
						else
						  continue;
					  else
						if(intBuffer[p + offset9] > cb)
						  if(intBuffer[p + offset10] > cb)
							if(intBuffer[p + offset11] > cb)
							  if(intBuffer[p + offset8] > cb)
								if(intBuffer[p + offset12] > cb)
								  if(intBuffer[p + offset13] > cb)
									if(intBuffer[p + offset14] > cb)
									  if(intBuffer[p + offset15] > cb)
										{}
									  else
										if(intBuffer[p + offset6] > cb)
										  if(intBuffer[p + offset7] > cb)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset5] > cb)
										if(intBuffer[p + offset6] > cb)
										  if(intBuffer[p + offset7] > cb)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset4] > cb)
									  if(intBuffer[p + offset5] > cb)
										if(intBuffer[p + offset6] > cb)
										  if(intBuffer[p + offset7] > cb)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset3] > cb)
									if(intBuffer[p + offset4] > cb)
									  if(intBuffer[p + offset5] > cb)
										if(intBuffer[p + offset6] > cb)
										  if(intBuffer[p + offset7] > cb)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								if(intBuffer[p + offset1] > cb)
								  if(intBuffer[p + offset12] > cb)
									if(intBuffer[p + offset13] > cb)
									  if(intBuffer[p + offset14] > cb)
										if(intBuffer[p + offset15] > cb)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							else
							  continue;
						  else
							continue;
						else if(intBuffer[p + offset9] < c_b)
						  if(intBuffer[p + offset7] < c_b)
							if(intBuffer[p + offset8] < c_b)
							  if(intBuffer[p + offset10] < c_b)
								if(intBuffer[p + offset11] < c_b)
								  if(intBuffer[p + offset6] < c_b)
									if(intBuffer[p + offset5] < c_b)
									  if(intBuffer[p + offset4] < c_b)
										if(intBuffer[p + offset3] < c_b)
										  {}
										else
										  if(intBuffer[p + offset12] < c_b)
											{}
										  else
											continue;
									  else
										if(intBuffer[p + offset12] < c_b)
										  if(intBuffer[p + offset13] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset12] < c_b)
										if(intBuffer[p + offset13] < c_b)
										  if(intBuffer[p + offset14] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset12] < c_b)
									  if(intBuffer[p + offset13] < c_b)
										if(intBuffer[p + offset14] < c_b)
										  if(intBuffer[p + offset15] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  continue;
							  else
								continue;
							else
							  continue;
						  else
							continue;
						else
						  continue;
					else if(intBuffer[p + offset0] < c_b)
					  if(intBuffer[p + offset2] > cb)
						if(intBuffer[p + offset9] > cb)
						  if(intBuffer[p + offset7] > cb)
							if(intBuffer[p + offset8] > cb)
							  if(intBuffer[p + offset6] > cb)
								if(intBuffer[p + offset5] > cb)
								  if(intBuffer[p + offset4] > cb)
									if(intBuffer[p + offset3] > cb)
									  if(intBuffer[p + offset1] > cb)
										{}
									  else
										if(intBuffer[p + offset10] > cb)
										  {}
										else
										  continue;
									else
									  if(intBuffer[p + offset10] > cb)
										if(intBuffer[p + offset11] > cb)
										  if(intBuffer[p + offset12] > cb)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset10] > cb)
									  if(intBuffer[p + offset11] > cb)
										if(intBuffer[p + offset12] > cb)
										  if(intBuffer[p + offset13] > cb)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset10] > cb)
									if(intBuffer[p + offset11] > cb)
									  if(intBuffer[p + offset12] > cb)
										if(intBuffer[p + offset13] > cb)
										  if(intBuffer[p + offset14] > cb)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								if(intBuffer[p + offset10] > cb)
								  if(intBuffer[p + offset11] > cb)
									if(intBuffer[p + offset12] > cb)
									  if(intBuffer[p + offset13] > cb)
										if(intBuffer[p + offset14] > cb)
										  if(intBuffer[p + offset15] > cb)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							else
							  continue;
						  else
							continue;
						else if(intBuffer[p + offset9] < c_b)
						  if(intBuffer[p + offset10] < c_b)
							if(intBuffer[p + offset11] < c_b)
							  if(intBuffer[p + offset8] < c_b)
								if(intBuffer[p + offset12] < c_b)
								  if(intBuffer[p + offset13] < c_b)
									if(intBuffer[p + offset14] < c_b)
									  if(intBuffer[p + offset15] < c_b)
										{}
									  else
										if(intBuffer[p + offset6] < c_b)
										  if(intBuffer[p + offset7] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset5] < c_b)
										if(intBuffer[p + offset6] < c_b)
										  if(intBuffer[p + offset7] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset4] < c_b)
									  if(intBuffer[p + offset5] < c_b)
										if(intBuffer[p + offset6] < c_b)
										  if(intBuffer[p + offset7] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset3] < c_b)
									if(intBuffer[p + offset4] < c_b)
									  if(intBuffer[p + offset5] < c_b)
										if(intBuffer[p + offset6] < c_b)
										  if(intBuffer[p + offset7] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								if(intBuffer[p + offset1] < c_b)
								  if(intBuffer[p + offset12] < c_b)
									if(intBuffer[p + offset13] < c_b)
									  if(intBuffer[p + offset14] < c_b)
										if(intBuffer[p + offset15] < c_b)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							else
							  continue;
						  else
							continue;
						else
						  continue;
					  else if(intBuffer[p + offset2] < c_b)
						if(intBuffer[p + offset4] > cb)
						  if(intBuffer[p + offset11] > cb)
							if(intBuffer[p + offset7] > cb)
							  if(intBuffer[p + offset8] > cb)
								if(intBuffer[p + offset9] > cb)
								  if(intBuffer[p + offset10] > cb)
									if(intBuffer[p + offset6] > cb)
									  if(intBuffer[p + offset5] > cb)
										if(intBuffer[p + offset3] > cb)
										  {}
										else
										  if(intBuffer[p + offset12] > cb)
											{}
										  else
											continue;
									  else
										if(intBuffer[p + offset12] > cb)
										  if(intBuffer[p + offset13] > cb)
											if(intBuffer[p + offset14] > cb)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset12] > cb)
										if(intBuffer[p + offset13] > cb)
										  if(intBuffer[p + offset14] > cb)
											if(intBuffer[p + offset15] > cb)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									continue;
								else
								  continue;
							  else
								continue;
							else
							  continue;
						  else if(intBuffer[p + offset11] < c_b)
							if(intBuffer[p + offset12] < c_b)
							  if(intBuffer[p + offset13] < c_b)
								if(intBuffer[p + offset10] < c_b)
								  if(intBuffer[p + offset14] < c_b)
									if(intBuffer[p + offset15] < c_b)
									  if(intBuffer[p + offset1] < c_b)
										{}
									  else
										if(intBuffer[p + offset8] < c_b)
										  if(intBuffer[p + offset9] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset6] < c_b)
										if(intBuffer[p + offset7] < c_b)
										  if(intBuffer[p + offset8] < c_b)
											if(intBuffer[p + offset9] < c_b)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset5] < c_b)
									  if(intBuffer[p + offset6] < c_b)
										if(intBuffer[p + offset7] < c_b)
										  if(intBuffer[p + offset8] < c_b)
											if(intBuffer[p + offset9] < c_b)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset1] < c_b)
									if(intBuffer[p + offset3] < c_b)
									  if(intBuffer[p + offset14] < c_b)
										if(intBuffer[p + offset15] < c_b)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								continue;
							else
							  continue;
						  else
							continue;
						else if(intBuffer[p + offset4] < c_b)
						  if(intBuffer[p + offset5] > cb)
							if(intBuffer[p + offset12] > cb)
							  if(intBuffer[p + offset7] > cb)
								if(intBuffer[p + offset8] > cb)
								  if(intBuffer[p + offset9] > cb)
									if(intBuffer[p + offset10] > cb)
									  if(intBuffer[p + offset11] > cb)
										if(intBuffer[p + offset13] > cb)
										  if(intBuffer[p + offset6] > cb)
											{}
										  else
											if(intBuffer[p + offset14] > cb)
											  if(intBuffer[p + offset15] > cb)
												{}
											  else
												continue;
											else
											  continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							  else
								continue;
							else if(intBuffer[p + offset12] < c_b)
							  if(intBuffer[p + offset13] < c_b)
								if(intBuffer[p + offset14] < c_b)
								  if(intBuffer[p + offset15] < c_b)
									if(intBuffer[p + offset1] < c_b)
									  if(intBuffer[p + offset3] < c_b)
										{}
									  else
										if(intBuffer[p + offset10] < c_b)
										  if(intBuffer[p + offset11] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset8] < c_b)
										if(intBuffer[p + offset9] < c_b)
										  if(intBuffer[p + offset10] < c_b)
											if(intBuffer[p + offset11] < c_b)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset6] < c_b)
									  if(intBuffer[p + offset7] < c_b)
										if(intBuffer[p + offset8] < c_b)
										  if(intBuffer[p + offset9] < c_b)
											if(intBuffer[p + offset10] < c_b)
											  if(intBuffer[p + offset11] < c_b)
												{}
											  else
												continue;
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  continue;
							  else
								continue;
							else
							  continue;
						  else if(intBuffer[p + offset5] < c_b)
							if(intBuffer[p + offset7] > cb)
							  if(intBuffer[p + offset14] > cb)
								if(intBuffer[p + offset8] > cb)
								  if(intBuffer[p + offset9] > cb)
									if(intBuffer[p + offset10] > cb)
									  if(intBuffer[p + offset11] > cb)
										if(intBuffer[p + offset12] > cb)
										  if(intBuffer[p + offset13] > cb)
											if(intBuffer[p + offset6] > cb)
											  {}
											else
											  if(intBuffer[p + offset15] > cb)
												{}
											  else
												continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							  else if(intBuffer[p + offset14] < c_b)
								if(intBuffer[p + offset15] < c_b)
								  if(intBuffer[p + offset1] < c_b)
									if(intBuffer[p + offset3] < c_b)
									  if(intBuffer[p + offset6] < c_b)
										{}
									  else
										if(intBuffer[p + offset13] < c_b)
										  {}
										else
										  continue;
									else
									  if(intBuffer[p + offset10] < c_b)
										if(intBuffer[p + offset11] < c_b)
										  if(intBuffer[p + offset12] < c_b)
											if(intBuffer[p + offset13] < c_b)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset8] < c_b)
									  if(intBuffer[p + offset9] < c_b)
										if(intBuffer[p + offset10] < c_b)
										  if(intBuffer[p + offset11] < c_b)
											if(intBuffer[p + offset12] < c_b)
											  if(intBuffer[p + offset13] < c_b)
												{}
											  else
												continue;
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  continue;
							  else
								continue;
							else if(intBuffer[p + offset7] < c_b)
							  if(intBuffer[p + offset3] < c_b)
								if(intBuffer[p + offset1] < c_b)
								  if(intBuffer[p + offset6] < c_b)
									if(intBuffer[p + offset8] < c_b)
									  {}
									else
									  if(intBuffer[p + offset15] < c_b)
										{}
									  else
										continue;
								  else
									if(intBuffer[p + offset13] < c_b)
									  if(intBuffer[p + offset14] < c_b)
										if(intBuffer[p + offset15] < c_b)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset8] < c_b)
									if(intBuffer[p + offset9] < c_b)
									  if(intBuffer[p + offset10] < c_b)
										if(intBuffer[p + offset6] < c_b)
										  {}
										else
										  if(intBuffer[p + offset11] < c_b)
											if(intBuffer[p + offset12] < c_b)
											  if(intBuffer[p + offset13] < c_b)
												if(intBuffer[p + offset14] < c_b)
												  if(intBuffer[p + offset15] < c_b)
													{}
												  else
													continue;
												else
												  continue;
											  else
												continue;
											else
											  continue;
										  else
											continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								if(intBuffer[p + offset10] < c_b)
								  if(intBuffer[p + offset11] < c_b)
									if(intBuffer[p + offset12] < c_b)
									  if(intBuffer[p + offset8] < c_b)
										if(intBuffer[p + offset9] < c_b)
										  if(intBuffer[p + offset6] < c_b)
											{}
										  else
											if(intBuffer[p + offset13] < c_b)
											  if(intBuffer[p + offset14] < c_b)
												if(intBuffer[p + offset15] < c_b)
												  {}
												else
												  continue;
											  else
												continue;
											else
											  continue;
										else
										  if(intBuffer[p + offset1] < c_b)
											if(intBuffer[p + offset13] < c_b)
											  if(intBuffer[p + offset14] < c_b)
												if(intBuffer[p + offset15] < c_b)
												  {}
												else
												  continue;
											  else
												continue;
											else
											  continue;
										  else
											continue;
									  else
										if(intBuffer[p + offset1] < c_b)
										  if(intBuffer[p + offset13] < c_b)
											if(intBuffer[p + offset14] < c_b)
											  if(intBuffer[p + offset15] < c_b)
												{}
											  else
												continue;
											else
											  continue;
										  else
											continue;
										else
										  continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							else
							  if(intBuffer[p + offset14] < c_b)
								if(intBuffer[p + offset15] < c_b)
								  if(intBuffer[p + offset1] < c_b)
									if(intBuffer[p + offset3] < c_b)
									  if(intBuffer[p + offset6] < c_b)
										{}
									  else
										if(intBuffer[p + offset13] < c_b)
										  {}
										else
										  continue;
									else
									  if(intBuffer[p + offset10] < c_b)
										if(intBuffer[p + offset11] < c_b)
										  if(intBuffer[p + offset12] < c_b)
											if(intBuffer[p + offset13] < c_b)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset8] < c_b)
									  if(intBuffer[p + offset9] < c_b)
										if(intBuffer[p + offset10] < c_b)
										  if(intBuffer[p + offset11] < c_b)
											if(intBuffer[p + offset12] < c_b)
											  if(intBuffer[p + offset13] < c_b)
												{}
											  else
												continue;
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  continue;
							  else
								continue;
						  else
							if(intBuffer[p + offset12] > cb)
							  if(intBuffer[p + offset7] > cb)
								if(intBuffer[p + offset8] > cb)
								  if(intBuffer[p + offset9] > cb)
									if(intBuffer[p + offset10] > cb)
									  if(intBuffer[p + offset11] > cb)
										if(intBuffer[p + offset13] > cb)
										  if(intBuffer[p + offset14] > cb)
											if(intBuffer[p + offset6] > cb)
											  {}
											else
											  if(intBuffer[p + offset15] > cb)
												{}
											  else
												continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							  else
								continue;
							else if(intBuffer[p + offset12] < c_b)
							  if(intBuffer[p + offset13] < c_b)
								if(intBuffer[p + offset14] < c_b)
								  if(intBuffer[p + offset15] < c_b)
									if(intBuffer[p + offset1] < c_b)
									  if(intBuffer[p + offset3] < c_b)
										{}
									  else
										if(intBuffer[p + offset10] < c_b)
										  if(intBuffer[p + offset11] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset8] < c_b)
										if(intBuffer[p + offset9] < c_b)
										  if(intBuffer[p + offset10] < c_b)
											if(intBuffer[p + offset11] < c_b)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset6] < c_b)
									  if(intBuffer[p + offset7] < c_b)
										if(intBuffer[p + offset8] < c_b)
										  if(intBuffer[p + offset9] < c_b)
											if(intBuffer[p + offset10] < c_b)
											  if(intBuffer[p + offset11] < c_b)
												{}
											  else
												continue;
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  continue;
							  else
								continue;
							else
							  continue;
						else
						  if(intBuffer[p + offset11] > cb)
							if(intBuffer[p + offset7] > cb)
							  if(intBuffer[p + offset8] > cb)
								if(intBuffer[p + offset9] > cb)
								  if(intBuffer[p + offset10] > cb)
									if(intBuffer[p + offset12] > cb)
									  if(intBuffer[p + offset13] > cb)
										if(intBuffer[p + offset6] > cb)
										  if(intBuffer[p + offset5] > cb)
											{}
										  else
											if(intBuffer[p + offset14] > cb)
											  {}
											else
											  continue;
										else
										  if(intBuffer[p + offset14] > cb)
											if(intBuffer[p + offset15] > cb)
											  {}
											else
											  continue;
										  else
											continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							  else
								continue;
							else
							  continue;
						  else if(intBuffer[p + offset11] < c_b)
							if(intBuffer[p + offset12] < c_b)
							  if(intBuffer[p + offset13] < c_b)
								if(intBuffer[p + offset10] < c_b)
								  if(intBuffer[p + offset14] < c_b)
									if(intBuffer[p + offset15] < c_b)
									  if(intBuffer[p + offset1] < c_b)
										{}
									  else
										if(intBuffer[p + offset8] < c_b)
										  if(intBuffer[p + offset9] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset6] < c_b)
										if(intBuffer[p + offset7] < c_b)
										  if(intBuffer[p + offset8] < c_b)
											if(intBuffer[p + offset9] < c_b)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset5] < c_b)
									  if(intBuffer[p + offset6] < c_b)
										if(intBuffer[p + offset7] < c_b)
										  if(intBuffer[p + offset8] < c_b)
											if(intBuffer[p + offset9] < c_b)
											  {}
											else
											  continue;
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset1] < c_b)
									if(intBuffer[p + offset3] < c_b)
									  if(intBuffer[p + offset14] < c_b)
										if(intBuffer[p + offset15] < c_b)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								continue;
							else
							  continue;
						  else
							continue;
					  else
						if(intBuffer[p + offset9] > cb)
						  if(intBuffer[p + offset7] > cb)
							if(intBuffer[p + offset8] > cb)
							  if(intBuffer[p + offset10] > cb)
								if(intBuffer[p + offset11] > cb)
								  if(intBuffer[p + offset6] > cb)
									if(intBuffer[p + offset5] > cb)
									  if(intBuffer[p + offset4] > cb)
										if(intBuffer[p + offset3] > cb)
										  {}
										else
										  if(intBuffer[p + offset12] > cb)
											{}
										  else
											continue;
									  else
										if(intBuffer[p + offset12] > cb)
										  if(intBuffer[p + offset13] > cb)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset12] > cb)
										if(intBuffer[p + offset13] > cb)
										  if(intBuffer[p + offset14] > cb)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset12] > cb)
									  if(intBuffer[p + offset13] > cb)
										if(intBuffer[p + offset14] > cb)
										  if(intBuffer[p + offset15] > cb)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  continue;
							  else
								continue;
							else
							  continue;
						  else
							continue;
						else if(intBuffer[p + offset9] < c_b)
						  if(intBuffer[p + offset10] < c_b)
							if(intBuffer[p + offset11] < c_b)
							  if(intBuffer[p + offset8] < c_b)
								if(intBuffer[p + offset12] < c_b)
								  if(intBuffer[p + offset13] < c_b)
									if(intBuffer[p + offset14] < c_b)
									  if(intBuffer[p + offset15] < c_b)
										{}
									  else
										if(intBuffer[p + offset6] < c_b)
										  if(intBuffer[p + offset7] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									else
									  if(intBuffer[p + offset5] < c_b)
										if(intBuffer[p + offset6] < c_b)
										  if(intBuffer[p + offset7] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset4] < c_b)
									  if(intBuffer[p + offset5] < c_b)
										if(intBuffer[p + offset6] < c_b)
										  if(intBuffer[p + offset7] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset3] < c_b)
									if(intBuffer[p + offset4] < c_b)
									  if(intBuffer[p + offset5] < c_b)
										if(intBuffer[p + offset6] < c_b)
										  if(intBuffer[p + offset7] < c_b)
											{}
										  else
											continue;
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								if(intBuffer[p + offset1] < c_b)
								  if(intBuffer[p + offset12] < c_b)
									if(intBuffer[p + offset13] < c_b)
									  if(intBuffer[p + offset14] < c_b)
										if(intBuffer[p + offset15] < c_b)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							else
							  continue;
						  else
							continue;
						else
						  continue;
					else
					  if(intBuffer[p + offset7] > cb)
						if(intBuffer[p + offset8] > cb)
						  if(intBuffer[p + offset9] > cb)
							if(intBuffer[p + offset6] > cb)
							  if(intBuffer[p + offset5] > cb)
								if(intBuffer[p + offset4] > cb)
								  if(intBuffer[p + offset3] > cb)
									if(intBuffer[p + offset2] > cb)
									  if(intBuffer[p + offset1] > cb)
										{}
									  else
										if(intBuffer[p + offset10] > cb)
										  {}
										else
										  continue;
									else
									  if(intBuffer[p + offset10] > cb)
										if(intBuffer[p + offset11] > cb)
										  {}
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset10] > cb)
									  if(intBuffer[p + offset11] > cb)
										if(intBuffer[p + offset12] > cb)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset10] > cb)
									if(intBuffer[p + offset11] > cb)
									  if(intBuffer[p + offset12] > cb)
										if(intBuffer[p + offset13] > cb)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								if(intBuffer[p + offset10] > cb)
								  if(intBuffer[p + offset11] > cb)
									if(intBuffer[p + offset12] > cb)
									  if(intBuffer[p + offset13] > cb)
										if(intBuffer[p + offset14] > cb)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							else
							  if(intBuffer[p + offset10] > cb)
								if(intBuffer[p + offset11] > cb)
								  if(intBuffer[p + offset12] > cb)
									if(intBuffer[p + offset13] > cb)
									  if(intBuffer[p + offset14] > cb)
										if(intBuffer[p + offset15] > cb)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							  else
								continue;
						  else
							continue;
						else
						  continue;
					  else if(intBuffer[p + offset7] < c_b)
						if(intBuffer[p + offset8] < c_b)
						  if(intBuffer[p + offset9] < c_b)
							if(intBuffer[p + offset6] < c_b)
							  if(intBuffer[p + offset5] < c_b)
								if(intBuffer[p + offset4] < c_b)
								  if(intBuffer[p + offset3] < c_b)
									if(intBuffer[p + offset2] < c_b)
									  if(intBuffer[p + offset1] < c_b)
										{}
									  else
										if(intBuffer[p + offset10] < c_b)
										  {}
										else
										  continue;
									else
									  if(intBuffer[p + offset10] < c_b)
										if(intBuffer[p + offset11] < c_b)
										  {}
										else
										  continue;
									  else
										continue;
								  else
									if(intBuffer[p + offset10] < c_b)
									  if(intBuffer[p + offset11] < c_b)
										if(intBuffer[p + offset12] < c_b)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								else
								  if(intBuffer[p + offset10] < c_b)
									if(intBuffer[p + offset11] < c_b)
									  if(intBuffer[p + offset12] < c_b)
										if(intBuffer[p + offset13] < c_b)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
							  else
								if(intBuffer[p + offset10] < c_b)
								  if(intBuffer[p + offset11] < c_b)
									if(intBuffer[p + offset12] < c_b)
									  if(intBuffer[p + offset13] < c_b)
										if(intBuffer[p + offset14] < c_b)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							else
							  if(intBuffer[p + offset10] < c_b)
								if(intBuffer[p + offset11] < c_b)
								  if(intBuffer[p + offset12] < c_b)
									if(intBuffer[p + offset13] < c_b)
									  if(intBuffer[p + offset14] < c_b)
										if(intBuffer[p + offset15] < c_b)
										  {}
										else
										  continue;
									  else
										continue;
									else
									  continue;
								  else
									continue;
								else
								  continue;
							  else
								continue;
						  else
							continue;
						else
						  continue;
					  else
						continue;
				}
				if(total == nExpectedCorners) 	
				{								
					if(nExpectedCorners==0)
					{
						nExpectedCorners=512;
						//corners_all.setSize(nExpectedCorners);
					}
					else
					{
						nExpectedCorners *=2;
						//corners_all.setSize(nExpectedCorners);
					}
				}
				h.x=x;
				h.y=y;
				corners_all.add(new Point2i(h));
				total++;
			}									
		}										
	}
	
	public int getCornerScore(int intBuffer[], int p)
	{
	    int bmax = 255;
		int bmin = threshold;
	    int b_test = (bmax + bmin)/2;

		int offset0=s_offset0;
		int offset1=s_offset1;
		int offset2=s_offset2;
		int offset3=s_offset3;
		int offset4=s_offset4;
		int offset5=s_offset5;
		int offset6=s_offset6;
		int offset7=s_offset7;
		int offset8=s_offset8;
		int offset9=s_offset9;
		int offset10=s_offset10;
		int offset11=s_offset11;
		int offset12=s_offset12;
		int offset13=s_offset13;
		int offset14=s_offset14;
		int offset15=s_offset15;

		while(true)
		{
		is_not_a_corner: while (true)
		{
		is_a_corner: while (true)
		{
			int cb = intBuffer[p] + b_test;
			int c_b = intBuffer[p] - b_test;
			if(intBuffer[p + offset0] > cb)
			  if(intBuffer[p + offset2] > cb)
			    if(intBuffer[p + offset4] > cb)
			      if(intBuffer[p + offset5] > cb)
			        if(intBuffer[p + offset7] > cb)
			          if(intBuffer[p + offset3] > cb)
			            if(intBuffer[p + offset1] > cb)
			              if(intBuffer[p + offset6] > cb)
			                if(intBuffer[p + offset8] > cb)
			                  break is_a_corner;
			                else
			                  if(intBuffer[p + offset15] > cb)
			                    break is_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset13] > cb)
			                  if(intBuffer[p + offset14] > cb)
			                    if(intBuffer[p + offset15] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset8] > cb)
			                if(intBuffer[p + offset9] > cb)
			                  if(intBuffer[p + offset10] > cb)
			                    if(intBuffer[p + offset6] > cb)
			                      break is_a_corner;
			                    else
			                      if(intBuffer[p + offset11] > cb)
			                        if(intBuffer[p + offset12] > cb)
			                          if(intBuffer[p + offset13] > cb)
			                            if(intBuffer[p + offset14] > cb)
			                              if(intBuffer[p + offset15] > cb)
			                                break is_a_corner;
			                              else
			                                break is_not_a_corner;
			                            else
			                              break is_not_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            if(intBuffer[p + offset10] > cb)
			              if(intBuffer[p + offset11] > cb)
			                if(intBuffer[p + offset12] > cb)
			                  if(intBuffer[p + offset8] > cb)
			                    if(intBuffer[p + offset9] > cb)
			                      if(intBuffer[p + offset6] > cb)
			                        break is_a_corner;
			                      else
			                        if(intBuffer[p + offset13] > cb)
			                          if(intBuffer[p + offset14] > cb)
			                            if(intBuffer[p + offset15] > cb)
			                              break is_a_corner;
			                            else
			                              break is_not_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                    else
			                      if(intBuffer[p + offset1] > cb)
			                        if(intBuffer[p + offset13] > cb)
			                          if(intBuffer[p + offset14] > cb)
			                            if(intBuffer[p + offset15] > cb)
			                              break is_a_corner;
			                            else
			                              break is_not_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                  else
			                    if(intBuffer[p + offset1] > cb)
			                      if(intBuffer[p + offset13] > cb)
			                        if(intBuffer[p + offset14] > cb)
			                          if(intBuffer[p + offset15] > cb)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			        else if(intBuffer[p + offset7] < c_b)
			          if(intBuffer[p + offset14] > cb)
			            if(intBuffer[p + offset15] > cb)
			              if(intBuffer[p + offset1] > cb)
			                if(intBuffer[p + offset3] > cb)
			                  if(intBuffer[p + offset6] > cb)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset13] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset10] > cb)
			                    if(intBuffer[p + offset11] > cb)
			                      if(intBuffer[p + offset12] > cb)
			                        if(intBuffer[p + offset13] > cb)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset8] > cb)
			                  if(intBuffer[p + offset9] > cb)
			                    if(intBuffer[p + offset10] > cb)
			                      if(intBuffer[p + offset11] > cb)
			                        if(intBuffer[p + offset12] > cb)
			                          if(intBuffer[p + offset13] > cb)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else if(intBuffer[p + offset14] < c_b)
			            if(intBuffer[p + offset8] < c_b)
			              if(intBuffer[p + offset9] < c_b)
			                if(intBuffer[p + offset10] < c_b)
			                  if(intBuffer[p + offset11] < c_b)
			                    if(intBuffer[p + offset12] < c_b)
			                      if(intBuffer[p + offset13] < c_b)
			                        if(intBuffer[p + offset6] < c_b)
			                          break is_a_corner;
			                        else
			                          if(intBuffer[p + offset15] < c_b)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          if(intBuffer[p + offset14] > cb)
			            if(intBuffer[p + offset15] > cb)
			              if(intBuffer[p + offset1] > cb)
			                if(intBuffer[p + offset3] > cb)
			                  if(intBuffer[p + offset6] > cb)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset13] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset10] > cb)
			                    if(intBuffer[p + offset11] > cb)
			                      if(intBuffer[p + offset12] > cb)
			                        if(intBuffer[p + offset13] > cb)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset8] > cb)
			                  if(intBuffer[p + offset9] > cb)
			                    if(intBuffer[p + offset10] > cb)
			                      if(intBuffer[p + offset11] > cb)
			                        if(intBuffer[p + offset12] > cb)
			                          if(intBuffer[p + offset13] > cb)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			      else if(intBuffer[p + offset5] < c_b)
			        if(intBuffer[p + offset12] > cb)
			          if(intBuffer[p + offset13] > cb)
			            if(intBuffer[p + offset14] > cb)
			              if(intBuffer[p + offset15] > cb)
			                if(intBuffer[p + offset1] > cb)
			                  if(intBuffer[p + offset3] > cb)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset10] > cb)
			                      if(intBuffer[p + offset11] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset8] > cb)
			                    if(intBuffer[p + offset9] > cb)
			                      if(intBuffer[p + offset10] > cb)
			                        if(intBuffer[p + offset11] > cb)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset6] > cb)
			                  if(intBuffer[p + offset7] > cb)
			                    if(intBuffer[p + offset8] > cb)
			                      if(intBuffer[p + offset9] > cb)
			                        if(intBuffer[p + offset10] > cb)
			                          if(intBuffer[p + offset11] > cb)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else if(intBuffer[p + offset12] < c_b)
			          if(intBuffer[p + offset7] < c_b)
			            if(intBuffer[p + offset8] < c_b)
			              if(intBuffer[p + offset9] < c_b)
			                if(intBuffer[p + offset10] < c_b)
			                  if(intBuffer[p + offset11] < c_b)
			                    if(intBuffer[p + offset13] < c_b)
			                      if(intBuffer[p + offset6] < c_b)
			                        break is_a_corner;
			                      else
			                        if(intBuffer[p + offset14] < c_b)
			                          if(intBuffer[p + offset15] < c_b)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        if(intBuffer[p + offset12] > cb)
			          if(intBuffer[p + offset13] > cb)
			            if(intBuffer[p + offset14] > cb)
			              if(intBuffer[p + offset15] > cb)
			                if(intBuffer[p + offset1] > cb)
			                  if(intBuffer[p + offset3] > cb)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset10] > cb)
			                      if(intBuffer[p + offset11] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset8] > cb)
			                    if(intBuffer[p + offset9] > cb)
			                      if(intBuffer[p + offset10] > cb)
			                        if(intBuffer[p + offset11] > cb)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset6] > cb)
			                  if(intBuffer[p + offset7] > cb)
			                    if(intBuffer[p + offset8] > cb)
			                      if(intBuffer[p + offset9] > cb)
			                        if(intBuffer[p + offset10] > cb)
			                          if(intBuffer[p + offset11] > cb)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else if(intBuffer[p + offset12] < c_b)
			          if(intBuffer[p + offset7] < c_b)
			            if(intBuffer[p + offset8] < c_b)
			              if(intBuffer[p + offset9] < c_b)
			                if(intBuffer[p + offset10] < c_b)
			                  if(intBuffer[p + offset11] < c_b)
			                    if(intBuffer[p + offset13] < c_b)
			                      if(intBuffer[p + offset14] < c_b)
			                        if(intBuffer[p + offset6] < c_b)
			                          break is_a_corner;
			                        else
			                          if(intBuffer[p + offset15] < c_b)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			    else if(intBuffer[p + offset4] < c_b)
			      if(intBuffer[p + offset11] > cb)
			        if(intBuffer[p + offset12] > cb)
			          if(intBuffer[p + offset13] > cb)
			            if(intBuffer[p + offset10] > cb)
			              if(intBuffer[p + offset14] > cb)
			                if(intBuffer[p + offset15] > cb)
			                  if(intBuffer[p + offset1] > cb)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset8] > cb)
			                      if(intBuffer[p + offset9] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset6] > cb)
			                    if(intBuffer[p + offset7] > cb)
			                      if(intBuffer[p + offset8] > cb)
			                        if(intBuffer[p + offset9] > cb)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset5] > cb)
			                  if(intBuffer[p + offset6] > cb)
			                    if(intBuffer[p + offset7] > cb)
			                      if(intBuffer[p + offset8] > cb)
			                        if(intBuffer[p + offset9] > cb)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset1] > cb)
			                if(intBuffer[p + offset3] > cb)
			                  if(intBuffer[p + offset14] > cb)
			                    if(intBuffer[p + offset15] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else if(intBuffer[p + offset11] < c_b)
			        if(intBuffer[p + offset7] < c_b)
			          if(intBuffer[p + offset8] < c_b)
			            if(intBuffer[p + offset9] < c_b)
			              if(intBuffer[p + offset10] < c_b)
			                if(intBuffer[p + offset6] < c_b)
			                  if(intBuffer[p + offset5] < c_b)
			                    if(intBuffer[p + offset3] < c_b)
			                      break is_a_corner;
			                    else
			                      if(intBuffer[p + offset12] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                  else
			                    if(intBuffer[p + offset12] < c_b)
			                      if(intBuffer[p + offset13] < c_b)
			                        if(intBuffer[p + offset14] < c_b)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset12] < c_b)
			                    if(intBuffer[p + offset13] < c_b)
			                      if(intBuffer[p + offset14] < c_b)
			                        if(intBuffer[p + offset15] < c_b)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        break is_not_a_corner;
			    else
			      if(intBuffer[p + offset11] > cb)
			        if(intBuffer[p + offset12] > cb)
			          if(intBuffer[p + offset13] > cb)
			            if(intBuffer[p + offset10] > cb)
			              if(intBuffer[p + offset14] > cb)
			                if(intBuffer[p + offset15] > cb)
			                  if(intBuffer[p + offset1] > cb)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset8] > cb)
			                      if(intBuffer[p + offset9] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset6] > cb)
			                    if(intBuffer[p + offset7] > cb)
			                      if(intBuffer[p + offset8] > cb)
			                        if(intBuffer[p + offset9] > cb)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset5] > cb)
			                  if(intBuffer[p + offset6] > cb)
			                    if(intBuffer[p + offset7] > cb)
			                      if(intBuffer[p + offset8] > cb)
			                        if(intBuffer[p + offset9] > cb)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset1] > cb)
			                if(intBuffer[p + offset3] > cb)
			                  if(intBuffer[p + offset14] > cb)
			                    if(intBuffer[p + offset15] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else if(intBuffer[p + offset11] < c_b)
			        if(intBuffer[p + offset7] < c_b)
			          if(intBuffer[p + offset8] < c_b)
			            if(intBuffer[p + offset9] < c_b)
			              if(intBuffer[p + offset10] < c_b)
			                if(intBuffer[p + offset12] < c_b)
			                  if(intBuffer[p + offset13] < c_b)
			                    if(intBuffer[p + offset6] < c_b)
			                      if(intBuffer[p + offset5] < c_b)
			                        break is_a_corner;
			                      else
			                        if(intBuffer[p + offset14] < c_b)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                    else
			                      if(intBuffer[p + offset14] < c_b)
			                        if(intBuffer[p + offset15] < c_b)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        break is_not_a_corner;
			  else if(intBuffer[p + offset2] < c_b)
			    if(intBuffer[p + offset9] > cb)
			      if(intBuffer[p + offset10] > cb)
			        if(intBuffer[p + offset11] > cb)
			          if(intBuffer[p + offset8] > cb)
			            if(intBuffer[p + offset12] > cb)
			              if(intBuffer[p + offset13] > cb)
			                if(intBuffer[p + offset14] > cb)
			                  if(intBuffer[p + offset15] > cb)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset6] > cb)
			                      if(intBuffer[p + offset7] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset5] > cb)
			                    if(intBuffer[p + offset6] > cb)
			                      if(intBuffer[p + offset7] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset4] > cb)
			                  if(intBuffer[p + offset5] > cb)
			                    if(intBuffer[p + offset6] > cb)
			                      if(intBuffer[p + offset7] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset3] > cb)
			                if(intBuffer[p + offset4] > cb)
			                  if(intBuffer[p + offset5] > cb)
			                    if(intBuffer[p + offset6] > cb)
			                      if(intBuffer[p + offset7] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            if(intBuffer[p + offset1] > cb)
			              if(intBuffer[p + offset12] > cb)
			                if(intBuffer[p + offset13] > cb)
			                  if(intBuffer[p + offset14] > cb)
			                    if(intBuffer[p + offset15] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        break is_not_a_corner;
			    else if(intBuffer[p + offset9] < c_b)
			      if(intBuffer[p + offset7] < c_b)
			        if(intBuffer[p + offset8] < c_b)
			          if(intBuffer[p + offset6] < c_b)
			            if(intBuffer[p + offset5] < c_b)
			              if(intBuffer[p + offset4] < c_b)
			                if(intBuffer[p + offset3] < c_b)
			                  if(intBuffer[p + offset1] < c_b)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset10] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset10] < c_b)
			                    if(intBuffer[p + offset11] < c_b)
			                      if(intBuffer[p + offset12] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset10] < c_b)
			                  if(intBuffer[p + offset11] < c_b)
			                    if(intBuffer[p + offset12] < c_b)
			                      if(intBuffer[p + offset13] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset10] < c_b)
			                if(intBuffer[p + offset11] < c_b)
			                  if(intBuffer[p + offset12] < c_b)
			                    if(intBuffer[p + offset13] < c_b)
			                      if(intBuffer[p + offset14] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            if(intBuffer[p + offset10] < c_b)
			              if(intBuffer[p + offset11] < c_b)
			                if(intBuffer[p + offset12] < c_b)
			                  if(intBuffer[p + offset13] < c_b)
			                    if(intBuffer[p + offset14] < c_b)
			                      if(intBuffer[p + offset15] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        break is_not_a_corner;
			    else
			      break is_not_a_corner;
			  else
			    if(intBuffer[p + offset9] > cb)
			      if(intBuffer[p + offset10] > cb)
			        if(intBuffer[p + offset11] > cb)
			          if(intBuffer[p + offset8] > cb)
			            if(intBuffer[p + offset12] > cb)
			              if(intBuffer[p + offset13] > cb)
			                if(intBuffer[p + offset14] > cb)
			                  if(intBuffer[p + offset15] > cb)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset6] > cb)
			                      if(intBuffer[p + offset7] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset5] > cb)
			                    if(intBuffer[p + offset6] > cb)
			                      if(intBuffer[p + offset7] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset4] > cb)
			                  if(intBuffer[p + offset5] > cb)
			                    if(intBuffer[p + offset6] > cb)
			                      if(intBuffer[p + offset7] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset3] > cb)
			                if(intBuffer[p + offset4] > cb)
			                  if(intBuffer[p + offset5] > cb)
			                    if(intBuffer[p + offset6] > cb)
			                      if(intBuffer[p + offset7] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            if(intBuffer[p + offset1] > cb)
			              if(intBuffer[p + offset12] > cb)
			                if(intBuffer[p + offset13] > cb)
			                  if(intBuffer[p + offset14] > cb)
			                    if(intBuffer[p + offset15] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        break is_not_a_corner;
			    else if(intBuffer[p + offset9] < c_b)
			      if(intBuffer[p + offset7] < c_b)
			        if(intBuffer[p + offset8] < c_b)
			          if(intBuffer[p + offset10] < c_b)
			            if(intBuffer[p + offset11] < c_b)
			              if(intBuffer[p + offset6] < c_b)
			                if(intBuffer[p + offset5] < c_b)
			                  if(intBuffer[p + offset4] < c_b)
			                    if(intBuffer[p + offset3] < c_b)
			                      break is_a_corner;
			                    else
			                      if(intBuffer[p + offset12] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                  else
			                    if(intBuffer[p + offset12] < c_b)
			                      if(intBuffer[p + offset13] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset12] < c_b)
			                    if(intBuffer[p + offset13] < c_b)
			                      if(intBuffer[p + offset14] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset12] < c_b)
			                  if(intBuffer[p + offset13] < c_b)
			                    if(intBuffer[p + offset14] < c_b)
			                      if(intBuffer[p + offset15] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        break is_not_a_corner;
			    else
			      break is_not_a_corner;
			else if(intBuffer[p + offset0] < c_b)
			  if(intBuffer[p + offset2] > cb)
			    if(intBuffer[p + offset9] > cb)
			      if(intBuffer[p + offset7] > cb)
			        if(intBuffer[p + offset8] > cb)
			          if(intBuffer[p + offset6] > cb)
			            if(intBuffer[p + offset5] > cb)
			              if(intBuffer[p + offset4] > cb)
			                if(intBuffer[p + offset3] > cb)
			                  if(intBuffer[p + offset1] > cb)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset10] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset10] > cb)
			                    if(intBuffer[p + offset11] > cb)
			                      if(intBuffer[p + offset12] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset10] > cb)
			                  if(intBuffer[p + offset11] > cb)
			                    if(intBuffer[p + offset12] > cb)
			                      if(intBuffer[p + offset13] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset10] > cb)
			                if(intBuffer[p + offset11] > cb)
			                  if(intBuffer[p + offset12] > cb)
			                    if(intBuffer[p + offset13] > cb)
			                      if(intBuffer[p + offset14] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            if(intBuffer[p + offset10] > cb)
			              if(intBuffer[p + offset11] > cb)
			                if(intBuffer[p + offset12] > cb)
			                  if(intBuffer[p + offset13] > cb)
			                    if(intBuffer[p + offset14] > cb)
			                      if(intBuffer[p + offset15] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        break is_not_a_corner;
			    else if(intBuffer[p + offset9] < c_b)
			      if(intBuffer[p + offset10] < c_b)
			        if(intBuffer[p + offset11] < c_b)
			          if(intBuffer[p + offset8] < c_b)
			            if(intBuffer[p + offset12] < c_b)
			              if(intBuffer[p + offset13] < c_b)
			                if(intBuffer[p + offset14] < c_b)
			                  if(intBuffer[p + offset15] < c_b)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset6] < c_b)
			                      if(intBuffer[p + offset7] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset5] < c_b)
			                    if(intBuffer[p + offset6] < c_b)
			                      if(intBuffer[p + offset7] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset4] < c_b)
			                  if(intBuffer[p + offset5] < c_b)
			                    if(intBuffer[p + offset6] < c_b)
			                      if(intBuffer[p + offset7] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset3] < c_b)
			                if(intBuffer[p + offset4] < c_b)
			                  if(intBuffer[p + offset5] < c_b)
			                    if(intBuffer[p + offset6] < c_b)
			                      if(intBuffer[p + offset7] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            if(intBuffer[p + offset1] < c_b)
			              if(intBuffer[p + offset12] < c_b)
			                if(intBuffer[p + offset13] < c_b)
			                  if(intBuffer[p + offset14] < c_b)
			                    if(intBuffer[p + offset15] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        break is_not_a_corner;
			    else
			      break is_not_a_corner;
			  else if(intBuffer[p + offset2] < c_b)
			    if(intBuffer[p + offset4] > cb)
			      if(intBuffer[p + offset11] > cb)
			        if(intBuffer[p + offset7] > cb)
			          if(intBuffer[p + offset8] > cb)
			            if(intBuffer[p + offset9] > cb)
			              if(intBuffer[p + offset10] > cb)
			                if(intBuffer[p + offset6] > cb)
			                  if(intBuffer[p + offset5] > cb)
			                    if(intBuffer[p + offset3] > cb)
			                      break is_a_corner;
			                    else
			                      if(intBuffer[p + offset12] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                  else
			                    if(intBuffer[p + offset12] > cb)
			                      if(intBuffer[p + offset13] > cb)
			                        if(intBuffer[p + offset14] > cb)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset12] > cb)
			                    if(intBuffer[p + offset13] > cb)
			                      if(intBuffer[p + offset14] > cb)
			                        if(intBuffer[p + offset15] > cb)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else if(intBuffer[p + offset11] < c_b)
			        if(intBuffer[p + offset12] < c_b)
			          if(intBuffer[p + offset13] < c_b)
			            if(intBuffer[p + offset10] < c_b)
			              if(intBuffer[p + offset14] < c_b)
			                if(intBuffer[p + offset15] < c_b)
			                  if(intBuffer[p + offset1] < c_b)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset8] < c_b)
			                      if(intBuffer[p + offset9] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset6] < c_b)
			                    if(intBuffer[p + offset7] < c_b)
			                      if(intBuffer[p + offset8] < c_b)
			                        if(intBuffer[p + offset9] < c_b)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset5] < c_b)
			                  if(intBuffer[p + offset6] < c_b)
			                    if(intBuffer[p + offset7] < c_b)
			                      if(intBuffer[p + offset8] < c_b)
			                        if(intBuffer[p + offset9] < c_b)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset1] < c_b)
			                if(intBuffer[p + offset3] < c_b)
			                  if(intBuffer[p + offset14] < c_b)
			                    if(intBuffer[p + offset15] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        break is_not_a_corner;
			    else if(intBuffer[p + offset4] < c_b)
			      if(intBuffer[p + offset5] > cb)
			        if(intBuffer[p + offset12] > cb)
			          if(intBuffer[p + offset7] > cb)
			            if(intBuffer[p + offset8] > cb)
			              if(intBuffer[p + offset9] > cb)
			                if(intBuffer[p + offset10] > cb)
			                  if(intBuffer[p + offset11] > cb)
			                    if(intBuffer[p + offset13] > cb)
			                      if(intBuffer[p + offset6] > cb)
			                        break is_a_corner;
			                      else
			                        if(intBuffer[p + offset14] > cb)
			                          if(intBuffer[p + offset15] > cb)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else if(intBuffer[p + offset12] < c_b)
			          if(intBuffer[p + offset13] < c_b)
			            if(intBuffer[p + offset14] < c_b)
			              if(intBuffer[p + offset15] < c_b)
			                if(intBuffer[p + offset1] < c_b)
			                  if(intBuffer[p + offset3] < c_b)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset10] < c_b)
			                      if(intBuffer[p + offset11] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset8] < c_b)
			                    if(intBuffer[p + offset9] < c_b)
			                      if(intBuffer[p + offset10] < c_b)
			                        if(intBuffer[p + offset11] < c_b)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset6] < c_b)
			                  if(intBuffer[p + offset7] < c_b)
			                    if(intBuffer[p + offset8] < c_b)
			                      if(intBuffer[p + offset9] < c_b)
			                        if(intBuffer[p + offset10] < c_b)
			                          if(intBuffer[p + offset11] < c_b)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else if(intBuffer[p + offset5] < c_b)
			        if(intBuffer[p + offset7] > cb)
			          if(intBuffer[p + offset14] > cb)
			            if(intBuffer[p + offset8] > cb)
			              if(intBuffer[p + offset9] > cb)
			                if(intBuffer[p + offset10] > cb)
			                  if(intBuffer[p + offset11] > cb)
			                    if(intBuffer[p + offset12] > cb)
			                      if(intBuffer[p + offset13] > cb)
			                        if(intBuffer[p + offset6] > cb)
			                          break is_a_corner;
			                        else
			                          if(intBuffer[p + offset15] > cb)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else if(intBuffer[p + offset14] < c_b)
			            if(intBuffer[p + offset15] < c_b)
			              if(intBuffer[p + offset1] < c_b)
			                if(intBuffer[p + offset3] < c_b)
			                  if(intBuffer[p + offset6] < c_b)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset13] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset10] < c_b)
			                    if(intBuffer[p + offset11] < c_b)
			                      if(intBuffer[p + offset12] < c_b)
			                        if(intBuffer[p + offset13] < c_b)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset8] < c_b)
			                  if(intBuffer[p + offset9] < c_b)
			                    if(intBuffer[p + offset10] < c_b)
			                      if(intBuffer[p + offset11] < c_b)
			                        if(intBuffer[p + offset12] < c_b)
			                          if(intBuffer[p + offset13] < c_b)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else if(intBuffer[p + offset7] < c_b)
			          if(intBuffer[p + offset3] < c_b)
			            if(intBuffer[p + offset1] < c_b)
			              if(intBuffer[p + offset6] < c_b)
			                if(intBuffer[p + offset8] < c_b)
			                  break is_a_corner;
			                else
			                  if(intBuffer[p + offset15] < c_b)
			                    break is_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset13] < c_b)
			                  if(intBuffer[p + offset14] < c_b)
			                    if(intBuffer[p + offset15] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset8] < c_b)
			                if(intBuffer[p + offset9] < c_b)
			                  if(intBuffer[p + offset10] < c_b)
			                    if(intBuffer[p + offset6] < c_b)
			                      break is_a_corner;
			                    else
			                      if(intBuffer[p + offset11] < c_b)
			                        if(intBuffer[p + offset12] < c_b)
			                          if(intBuffer[p + offset13] < c_b)
			                            if(intBuffer[p + offset14] < c_b)
			                              if(intBuffer[p + offset15] < c_b)
			                                break is_a_corner;
			                              else
			                                break is_not_a_corner;
			                            else
			                              break is_not_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            if(intBuffer[p + offset10] < c_b)
			              if(intBuffer[p + offset11] < c_b)
			                if(intBuffer[p + offset12] < c_b)
			                  if(intBuffer[p + offset8] < c_b)
			                    if(intBuffer[p + offset9] < c_b)
			                      if(intBuffer[p + offset6] < c_b)
			                        break is_a_corner;
			                      else
			                        if(intBuffer[p + offset13] < c_b)
			                          if(intBuffer[p + offset14] < c_b)
			                            if(intBuffer[p + offset15] < c_b)
			                              break is_a_corner;
			                            else
			                              break is_not_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                    else
			                      if(intBuffer[p + offset1] < c_b)
			                        if(intBuffer[p + offset13] < c_b)
			                          if(intBuffer[p + offset14] < c_b)
			                            if(intBuffer[p + offset15] < c_b)
			                              break is_a_corner;
			                            else
			                              break is_not_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                  else
			                    if(intBuffer[p + offset1] < c_b)
			                      if(intBuffer[p + offset13] < c_b)
			                        if(intBuffer[p + offset14] < c_b)
			                          if(intBuffer[p + offset15] < c_b)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			        else
			          if(intBuffer[p + offset14] < c_b)
			            if(intBuffer[p + offset15] < c_b)
			              if(intBuffer[p + offset1] < c_b)
			                if(intBuffer[p + offset3] < c_b)
			                  if(intBuffer[p + offset6] < c_b)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset13] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset10] < c_b)
			                    if(intBuffer[p + offset11] < c_b)
			                      if(intBuffer[p + offset12] < c_b)
			                        if(intBuffer[p + offset13] < c_b)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset8] < c_b)
			                  if(intBuffer[p + offset9] < c_b)
			                    if(intBuffer[p + offset10] < c_b)
			                      if(intBuffer[p + offset11] < c_b)
			                        if(intBuffer[p + offset12] < c_b)
			                          if(intBuffer[p + offset13] < c_b)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			      else
			        if(intBuffer[p + offset12] > cb)
			          if(intBuffer[p + offset7] > cb)
			            if(intBuffer[p + offset8] > cb)
			              if(intBuffer[p + offset9] > cb)
			                if(intBuffer[p + offset10] > cb)
			                  if(intBuffer[p + offset11] > cb)
			                    if(intBuffer[p + offset13] > cb)
			                      if(intBuffer[p + offset14] > cb)
			                        if(intBuffer[p + offset6] > cb)
			                          break is_a_corner;
			                        else
			                          if(intBuffer[p + offset15] > cb)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else if(intBuffer[p + offset12] < c_b)
			          if(intBuffer[p + offset13] < c_b)
			            if(intBuffer[p + offset14] < c_b)
			              if(intBuffer[p + offset15] < c_b)
			                if(intBuffer[p + offset1] < c_b)
			                  if(intBuffer[p + offset3] < c_b)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset10] < c_b)
			                      if(intBuffer[p + offset11] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset8] < c_b)
			                    if(intBuffer[p + offset9] < c_b)
			                      if(intBuffer[p + offset10] < c_b)
			                        if(intBuffer[p + offset11] < c_b)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset6] < c_b)
			                  if(intBuffer[p + offset7] < c_b)
			                    if(intBuffer[p + offset8] < c_b)
			                      if(intBuffer[p + offset9] < c_b)
			                        if(intBuffer[p + offset10] < c_b)
			                          if(intBuffer[p + offset11] < c_b)
			                            break is_a_corner;
			                          else
			                            break is_not_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			    else
			      if(intBuffer[p + offset11] > cb)
			        if(intBuffer[p + offset7] > cb)
			          if(intBuffer[p + offset8] > cb)
			            if(intBuffer[p + offset9] > cb)
			              if(intBuffer[p + offset10] > cb)
			                if(intBuffer[p + offset12] > cb)
			                  if(intBuffer[p + offset13] > cb)
			                    if(intBuffer[p + offset6] > cb)
			                      if(intBuffer[p + offset5] > cb)
			                        break is_a_corner;
			                      else
			                        if(intBuffer[p + offset14] > cb)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                    else
			                      if(intBuffer[p + offset14] > cb)
			                        if(intBuffer[p + offset15] > cb)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else if(intBuffer[p + offset11] < c_b)
			        if(intBuffer[p + offset12] < c_b)
			          if(intBuffer[p + offset13] < c_b)
			            if(intBuffer[p + offset10] < c_b)
			              if(intBuffer[p + offset14] < c_b)
			                if(intBuffer[p + offset15] < c_b)
			                  if(intBuffer[p + offset1] < c_b)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset8] < c_b)
			                      if(intBuffer[p + offset9] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset6] < c_b)
			                    if(intBuffer[p + offset7] < c_b)
			                      if(intBuffer[p + offset8] < c_b)
			                        if(intBuffer[p + offset9] < c_b)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset5] < c_b)
			                  if(intBuffer[p + offset6] < c_b)
			                    if(intBuffer[p + offset7] < c_b)
			                      if(intBuffer[p + offset8] < c_b)
			                        if(intBuffer[p + offset9] < c_b)
			                          break is_a_corner;
			                        else
			                          break is_not_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset1] < c_b)
			                if(intBuffer[p + offset3] < c_b)
			                  if(intBuffer[p + offset14] < c_b)
			                    if(intBuffer[p + offset15] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        break is_not_a_corner;
			  else
			    if(intBuffer[p + offset9] > cb)
			      if(intBuffer[p + offset7] > cb)
			        if(intBuffer[p + offset8] > cb)
			          if(intBuffer[p + offset10] > cb)
			            if(intBuffer[p + offset11] > cb)
			              if(intBuffer[p + offset6] > cb)
			                if(intBuffer[p + offset5] > cb)
			                  if(intBuffer[p + offset4] > cb)
			                    if(intBuffer[p + offset3] > cb)
			                      break is_a_corner;
			                    else
			                      if(intBuffer[p + offset12] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                  else
			                    if(intBuffer[p + offset12] > cb)
			                      if(intBuffer[p + offset13] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset12] > cb)
			                    if(intBuffer[p + offset13] > cb)
			                      if(intBuffer[p + offset14] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset12] > cb)
			                  if(intBuffer[p + offset13] > cb)
			                    if(intBuffer[p + offset14] > cb)
			                      if(intBuffer[p + offset15] > cb)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        break is_not_a_corner;
			    else if(intBuffer[p + offset9] < c_b)
			      if(intBuffer[p + offset10] < c_b)
			        if(intBuffer[p + offset11] < c_b)
			          if(intBuffer[p + offset8] < c_b)
			            if(intBuffer[p + offset12] < c_b)
			              if(intBuffer[p + offset13] < c_b)
			                if(intBuffer[p + offset14] < c_b)
			                  if(intBuffer[p + offset15] < c_b)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset6] < c_b)
			                      if(intBuffer[p + offset7] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset5] < c_b)
			                    if(intBuffer[p + offset6] < c_b)
			                      if(intBuffer[p + offset7] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset4] < c_b)
			                  if(intBuffer[p + offset5] < c_b)
			                    if(intBuffer[p + offset6] < c_b)
			                      if(intBuffer[p + offset7] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset3] < c_b)
			                if(intBuffer[p + offset4] < c_b)
			                  if(intBuffer[p + offset5] < c_b)
			                    if(intBuffer[p + offset6] < c_b)
			                      if(intBuffer[p + offset7] < c_b)
			                        break is_a_corner;
			                      else
			                        break is_not_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            if(intBuffer[p + offset1] < c_b)
			              if(intBuffer[p + offset12] < c_b)
			                if(intBuffer[p + offset13] < c_b)
			                  if(intBuffer[p + offset14] < c_b)
			                    if(intBuffer[p + offset15] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			        else
			          break is_not_a_corner;
			      else
			        break is_not_a_corner;
			    else
			      break is_not_a_corner;
			else
			  if(intBuffer[p + offset7] > cb)
			    if(intBuffer[p + offset8] > cb)
			      if(intBuffer[p + offset9] > cb)
			        if(intBuffer[p + offset6] > cb)
			          if(intBuffer[p + offset5] > cb)
			            if(intBuffer[p + offset4] > cb)
			              if(intBuffer[p + offset3] > cb)
			                if(intBuffer[p + offset2] > cb)
			                  if(intBuffer[p + offset1] > cb)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset10] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset10] > cb)
			                    if(intBuffer[p + offset11] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset10] > cb)
			                  if(intBuffer[p + offset11] > cb)
			                    if(intBuffer[p + offset12] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset10] > cb)
			                if(intBuffer[p + offset11] > cb)
			                  if(intBuffer[p + offset12] > cb)
			                    if(intBuffer[p + offset13] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            if(intBuffer[p + offset10] > cb)
			              if(intBuffer[p + offset11] > cb)
			                if(intBuffer[p + offset12] > cb)
			                  if(intBuffer[p + offset13] > cb)
			                    if(intBuffer[p + offset14] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			        else
			          if(intBuffer[p + offset10] > cb)
			            if(intBuffer[p + offset11] > cb)
			              if(intBuffer[p + offset12] > cb)
			                if(intBuffer[p + offset13] > cb)
			                  if(intBuffer[p + offset14] > cb)
			                    if(intBuffer[p + offset15] > cb)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			      else
			        break is_not_a_corner;
			    else
			      break is_not_a_corner;
			  else if(intBuffer[p + offset7] < c_b)
			    if(intBuffer[p + offset8] < c_b)
			      if(intBuffer[p + offset9] < c_b)
			        if(intBuffer[p + offset6] < c_b)
			          if(intBuffer[p + offset5] < c_b)
			            if(intBuffer[p + offset4] < c_b)
			              if(intBuffer[p + offset3] < c_b)
			                if(intBuffer[p + offset2] < c_b)
			                  if(intBuffer[p + offset1] < c_b)
			                    break is_a_corner;
			                  else
			                    if(intBuffer[p + offset10] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                else
			                  if(intBuffer[p + offset10] < c_b)
			                    if(intBuffer[p + offset11] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			              else
			                if(intBuffer[p + offset10] < c_b)
			                  if(intBuffer[p + offset11] < c_b)
			                    if(intBuffer[p + offset12] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			            else
			              if(intBuffer[p + offset10] < c_b)
			                if(intBuffer[p + offset11] < c_b)
			                  if(intBuffer[p + offset12] < c_b)
			                    if(intBuffer[p + offset13] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			          else
			            if(intBuffer[p + offset10] < c_b)
			              if(intBuffer[p + offset11] < c_b)
			                if(intBuffer[p + offset12] < c_b)
			                  if(intBuffer[p + offset13] < c_b)
			                    if(intBuffer[p + offset14] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			        else
			          if(intBuffer[p + offset10] < c_b)
			            if(intBuffer[p + offset11] < c_b)
			              if(intBuffer[p + offset12] < c_b)
			                if(intBuffer[p + offset13] < c_b)
			                  if(intBuffer[p + offset14] < c_b)
			                    if(intBuffer[p + offset15] < c_b)
			                      break is_a_corner;
			                    else
			                      break is_not_a_corner;
			                  else
			                    break is_not_a_corner;
			                else
			                  break is_not_a_corner;
			              else
			                break is_not_a_corner;
			            else
			              break is_not_a_corner;
			          else
			            break is_not_a_corner;
			      else
			        break is_not_a_corner;
			    else
			      break is_not_a_corner;
			  else
			    break is_not_a_corner;

			} // is_a_corner: while(true)
				bmin=b_test;
				if(bmax - bmin <= 1)
				return bmin;
			     b_test = (bmin + bmax) / 2;

		  } //	is_not_a_corner: while (true)
				bmax=b_test;
				if(bmax - bmin <= 1)
					return bmin;
				b_test = (bmin + bmax) / 2;
		} // while (true)
	}




}