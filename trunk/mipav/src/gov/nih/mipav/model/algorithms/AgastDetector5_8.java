package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

/**
agast5 - AGAST, an adaptive and generic corner detector based on the
         accelerated segment test for a 8 pixel mask

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


public class AgastDetector5_8 {
	
	
	private int xsize = 0;
	
	private int ysize = 0;
	
	private double threshold = -1;
	
	private int  s_offset0;
	private int  s_offset1;
	private int  s_offset2;
	private int  s_offset3;
	private int  s_offset4;
	private int  s_offset5;
	private int  s_offset6;
	private int  s_offset7;
	
	public AgastDetector5_8(int xsize, int ysize, double threshold) {
	    this.xsize = xsize;
	    this.ysize = ysize;
	    this.threshold = threshold;
	    init_pattern();
	}
	
	private void init_pattern()
	{
		s_offset0=(-1)+(0)*xsize;
		s_offset1=(-1)+(-1)*xsize;
		s_offset2=(0)+(-1)*xsize;
		s_offset3=(1)+(-1)*xsize;
		s_offset4=(1)+(0)*xsize;
		s_offset5=(1)+(1)*xsize;
		s_offset6=(0)+(1)*xsize;
		s_offset7=(-1)+(1)*xsize;
	
	}
	
	public void setThreshold(double threshold) {
		this.threshold = threshold;
	}
}