package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ShapeSimilarity;

import java.io.*;

/**
 *  Ported to Java from C code from 
 *  http://www8.cs.umu.se/kurser/TDBAfl/VT06/algorithms/WEBSITE/IMPLEMEN/TURN/IMPLEMEN.HTM
 *  May only be used for non-commercial purposes.
 *  
 *  Implementation of "An Efficiently Computable Metric
 *  for Comparing Polygonal Shapes," by Arkin, Chew, Huttenlocher,
 *  Kedem, and Mitchel (undated).  This expands a little on the
 *  cited reference to achieve O(n) space and O(mn log n)
 *  run time.
 *
 *  This could be improved to O(min m,n) and O(mn log min m,n)
 *  by selecting the smallest of the 2 polys to create the initial
 *  event heap.  See init_events().
 *
 *  Variable names match the article.
 *
 *  Implementation (c) Eugene K. Ressler 91, 92  This source may be
 *  freely distributed and used for non-commercial purposes, so long 
 *  as this comment is attached to any code copied or derived from it.
 *  
 * @author Ruida Cheng
 *
 */
public class ShapeSimilarity {
	
	EVENT HEAP[] = new EVENT[poly.MAX_PTS + 1];
	EVENT events[] = new EVENT[poly.MAX_PTS + 1];
	private BufferedReader bufferReader;
	
	public static int n_events = 0;
	
	
	public double  tr_n(TURN_REP tr) {
		return tr.n;
	}
	
	public double tr_i(TURN_REP tr, int i)  {
		return (i % tr.n);
	}
	
	
	public double tr_len(TURN_REP tr, int i)  { 
		return tr.leg[(int)tr_i(tr, i)].len;
	}
	
	public double tr_s(TURN_REP tr, int i)
	{
	    return(tr.leg[i % tr.n].s + i / tr.n);
	}

	public double tr_smt(TURN_REP tr, int i, double t)
	{
	    return((tr.leg[i % tr.n].s - t) + i / tr.n);
	}

	public double tr_theta(TURN_REP tr, int i)
	{
	    return(tr.leg[i % tr.n].theta + i / tr.n * 2 * Math.PI);
	}

	/*
	 * Square a double.
	 */
	public double sqr(double x)
	{
	    return(x * x);
	}

	/*
	 * Compute min of two ints.
	 */
	public int min(int x, int y)
	{
	    return(x < y ? x : y);
	}

	/*
	 * Compute floor(log_base2(x))
	 */
	public int ilog2(int x)
	{
	    int l;

	    l = -1;
	    while (x != 0) {
	        x >>= 1;
	        ++l;
	    }
	    return(l);
	}
	

	/*
	 * Return angle a, adjusted by +-2kPI so that it
	 * is within [base-PI, base+PI).
	 */
	public double turn(double a, double base)
	{
	    while (a - base < -Math.PI) a += 2 * Math.PI;
	    while (a - base >= Math.PI) a -= 2 * Math.PI;
	    return(a);
	}
	
	/*
	 * Convert a polygon to a turning rep.  This computes the
	 * absolute angle of each leg wrt the x-axis, then adjusts
	 * this to within PI of the last leg to form the turning
	 * angle.  Finally, the total length of all legs is used
	 * to compute the cumulative normalized arc length of each
	 * discontinuity, s.
	 */
	void poly_to_turn_rep(poly p, TURN_REP t)
	{
	    int n, i0, i1;
	    double dx, dy, theta1, theta0, total_len, len;

	    t.n = p.n;
	    n = t.n;
	    total_len = 0;
	    for (theta0 = 0, i0 = 0; i0 < n; theta0 = theta1, ++i0) {
	        /*
	         * Look one vertex ahead of i0 to compute the leg.
	         */
	        i1 = (i0 + 1) % n;
	        dx = p.pt[i1].x - p.pt[i0].x;
	        dy = p.pt[i1].y - p.pt[i0].y;
	        t.leg[i0] = new LEG();
	        t.leg[i0].theta = theta1 = turn(Math.atan2(dy, dx), theta0);
	        total_len += t.leg[i0].len = Math.sqrt(dx * dx + dy * dy);
	    }
	    t.total_len = total_len;
	    for (len = 0, i0 = 0; i0 < n; ++i0) {
	        t.leg[i0].s = len/total_len;
	        len += t.leg[i0].len;
	    }
	}
	
	/*
	 * Fill in a turn rep with a rotated version of an
	 * original.  Normalized arc lengths start at 0 in
	 * the new representation.
	 */
	void rotate_turn_rep(TURN_REP t, int to, TURN_REP r)
	{
	    int ti, ri, n;
	    double len, total_len;
	    LEG l;

	    n = r.n = t.n;
	    total_len = r.total_len = t.total_len;
	    for (ti = to, ri = 0; ri < n; ++ti, ++ri) {
	        l = r.leg[ri];
	        l.theta = tr_theta(t, ti);
	        l.len = tr_len(t, ti);
	        l.s = tr_s(t, ti);
	    }
	    for (len = 0, ri = 0; ri < n; ++ri) {
	        r.leg[ri].s = len/total_len;
	        len += r.leg[ri].len;
	    }
	}
	
	/*
	 * In one O(m + n) pass over the turning reps of the polygons
	 * to be matched, this computes all the terms needed to incrementally
	 * compute the metric.  See the paper.
	 */
	void init_vals(TURN_REP f, TURN_REP g, double []ht0_rtn, double []slope_rtn, double []a_rtn)
	{
	    int i, n;           /* loop params */
	    int fi, gi;         /* disconts that bound current strip */
	    double ht0, slope;  /* per paper */
	    double a;           /* alpha in the paper */
	    double last_s;      /* s at left edge of current strip */
	    double ds;          /* width of strip */
	    double dtheta;      /* height of strip */

	    last_s = 0;
	    /*
	     * First strip is between 0 and the
	     * earliest of 1'th f and g disconts.
	     */
	    gi = 1; fi = 1;
	    /*
	     * Zero accumulators and compute initial slope.
	     */
	    ht0 = a = 0;
	    slope = (tr_s(g, 1) < tr_s(f, 1)) ? 0 : -sqr(tr_theta(g, 0) - tr_theta(f, 0));
	    /*
	     * Count all the strips
	     */
	    for (i = 0, n = (int)(tr_n(f) + tr_n(g) - 1); i < n; ++i) {
	        /*
	         * Compute height of current strip.
	         */
	        dtheta = tr_theta(g, gi-1) - tr_theta(f, fi-1);
	        /*
	         * Determine flavor of discontinuity on right.
	         */
	        if (tr_s(f, fi) <= tr_s(g, gi)) {
	            /*
	             * It's f. Compute width of current strip,
	             * then bump area accumulators.
	             */
	            ds = tr_s(f, fi) - last_s;
	            a += ds * dtheta;
	            ht0 += ds * dtheta * dtheta;
	            /*
	             * Determine flavor of next strip.  We know it's ff
	             * or fg.  In latter case, bump accumulator.  Note
	             * we've skipped the first strip.  It's added as the
	             * "next" of the last strip.
	             */
	            if (tr_s(f, fi+1) > tr_s(g, gi))
	                slope += sqr(tr_theta(f, fi) - tr_theta(g, gi-1));
	            /*
	             * Go to next f discontinuity.
	             */
	            last_s = tr_s(f, fi++);
	        }
	        else {
	            /*
	             * Else it's g ...
	             */
	            ds = tr_s(g, gi) - last_s;
	            a    += ds * dtheta;
	            ht0 += ds * dtheta * dtheta;
	            /*
	             * ... and next strip is gg or gf, and again
	             * we're interested in the latter case.
	             */
	            if (tr_s(g, gi+1) >= tr_s(f, fi))
	                slope -= sqr(tr_theta(g, gi) - tr_theta(f, fi-1));
	            /*
	             * Go to next g discontinuity.
	             */
	            last_s = tr_s(g, gi++);
	        }
	    }
	    /*
	     * Set up all return values.
	     */
	    ht0_rtn[0] = ht0; 
	    slope_rtn[0] = slope; 
	    a_rtn[0] = a;
	}

	
	public void reinit_vals(TURN_REP f, TURN_REP g, int fi, int gi, double []ht0_rtn, double []slope_rtn)
	{
	    double[] a = new double[1];
	    TURN_REP fr = new TURN_REP();
	    TURN_REP gr = new TURN_REP();

	    rotate_turn_rep(f, fi, fr);
	    rotate_turn_rep(g, gi, gr);
	    init_vals(fr, gr, ht0_rtn, slope_rtn, a);
	}
	
	
	public int reinit_interval(TURN_REP f, TURN_REP g)
	{
	  return (int)(tr_n(f) * tr_n(g) / (Math.min(tr_n(f), tr_n(g)) * ilog2((int)tr_n(g))));
	}

	
	public void add_event(TURN_REP f, TURN_REP g, int fi, int gi)
	{
	    double t;
	    int i, j;
	    EVENT e;

	    if ((t = tr_s(f, fi) - tr_s(g, gi)) > 1)
	        return;
	    j = ++n_events;
	    i = n_events/2;
	    while (i > 0 && events[i].t > t) {
	        events[j] = events[i];
	        j = i;
	        i = i/2;
	    }
	    // e = events[j];
	    events[j] = new EVENT();
	    events[j].t = t;
	    events[j].fi = fi;
	    events[j].gi = gi;
	}

	/*
	 * Remove the event of min t from the heap and return it.
	 */
	public EVENT next_event()
	{
	    int i;
	    EVENT next = events[1];      /* remember event to return */
	    EVENT e = events[n_events];  /* new root (before adjust) */

	    --n_events;
	    i = 2;
	    while (i <= n_events) {
	        if (i < n_events && events[i].t > events[i+1].t)
	            ++i;
	        if (e.t <= events[i].t)
	            break;
	        else {
	            events[i/2] = events[i];
	            i *= 2;
	        }
	    }
	    events[i/2] = e;
	    return(next);
	}
	
	/*
	 * Peek at the next t to come off the heap without
	 * removing the element.
	 */
    public double next_t() {
    	return events[1].t;
    }
    
    
    public void init_events(TURN_REP f, TURN_REP g)
    {
        int fi, gi;

        n_events = 0;
        /*
         * Cycle through all g discontinuities, including
         * the one at s = 1.  This takes care of s = 0.
         */
        for (fi = gi = 1; gi <= tr_n(g); ++gi) {
            /*
             * Look for the first f discontinuity to the
             * right of this g discontinuity.
             */
            while (tr_s(f, fi) <= tr_s(g, gi))
                ++fi;
            add_event(f, g, fi, gi);
        }
    }
	
	
  public double h_t0min(TURN_REP f, TURN_REP g,
 	       double hc0, double slope, double alpha, int d_update,
                double []theta_star_rtn, EVENT[] e_rtn,
                double []hc0_err_rtn, double []slope_err_rtn)
 {
     int left_to_update;                 /* # disconts left until update */
     double metric2, min_metric2;        /* d^2 and d^2_min thus far */
     double theta_star, min_theta_star;  /* theta* and theta*_min thus far */
     double last_t;                      /* t of last iteration */
     double hc0_err, slope_err;          /* error mags discovered on update */
     EVENT e;                        /* current event */
     EVENT min_e;                    /* event of d^2_min thus far */

     EVENT init_min_e = new EVENT(0,0,0);  /* implicit first event */

     /*
      * At t = 0, theta_star is just alpha, and the min
      * metric2 seen so far is hc0 - min_theta_star^2.
      * Also, no error has been seen.
      */
     min_theta_star = alpha;
     min_metric2 = hc0 - min_theta_star * min_theta_star;
     min_e = init_min_e;
     last_t = hc0_err = slope_err = 0;
     /*
      * Compute successive hc_i0's by incremental update
      * at critical events as described in the paper.
      */
     left_to_update = d_update;
     while (n_events > 0) {
         e = next_event();
         hc0 += (e.t - last_t) * slope;
         theta_star = alpha - 2 * Math.PI * e.t;
         metric2 = hc0 - theta_star * theta_star;
         if (metric2 < min_metric2) {
             min_metric2 = metric2;
             min_theta_star = theta_star;
             min_e = e;
         }
         /*
          * Update slope, last t, and put next event for this g
          * discontinuity in the heap.
          */
         slope += 2*(tr_theta(f, e.fi-1) - tr_theta(f, e.fi))
                   *(tr_theta(g, e.gi-1) - tr_theta(g, e.gi));
         last_t = e.t;
         add_event(f, g, e.fi+1, e.gi);
         /*
          * Re-establish hc0 and slope now and then
          * to reduce numerical error.  If d_update is 0, do nothing.
          * Note we don't update if an event is close, as this
          * causes numerical ambiguity.  The test number could be much
          * smaller, but why tempt Murphey?  We force an update on last
          * event so there's always at least one.
          */
         if (d_update != 0 && 
 	    (n_events == 0 || 
             --left_to_update <= 0 && e.t - last_t > 0.001 && next_t() - e.t > 0.001)) {

             double[] rihc0 = new double[1];
             double[] rislope = new double[1];
             double dhc0, dslope;

             reinit_vals(f, g, e.fi, e.gi, rihc0, rislope);
             dhc0 = hc0 - rihc0[0];
             dslope = slope - rislope[0];
             if (Math.abs(dhc0) > Math.abs(hc0_err))
               hc0_err = dhc0;
             if (Math.abs(dslope) > Math.abs(slope_err))
               slope_err = dslope;
             hc0 = rihc0[0];
             slope = rislope[0];
             left_to_update = d_update;
         }
     }
     /*
      * Set up return values.
      */
     theta_star_rtn[0] = min_theta_star;
     e_rtn[0] = min_e;                             /// ???????????????????????   event
     hc0_err_rtn[0] = hc0_err;
     slope_err_rtn[0] = slope_err;
     return(min_metric2);
 }

  
	public void comparePolygon(poly pg, poly pf, double[] result) {
		int update_p, precise_p, n_repeats, i;
		TURN_REP trf = new TURN_REP();
		TURN_REP trg = new TURN_REP();
		TURN_REP f = new TURN_REP();
		TURN_REP g = new TURN_REP();
	
		// poly pf = new poly();
		// poly pg = new poly();
		EVENT[] e = new EVENT[1];
		e[0] = new EVENT();
		double[] ht0 = new double[1];
		double[] slope = new double[1];
		double[] alpha = new double[1];
		double[] theta_star = new double[1];
		double metric2 = 0, metric;
		double[] ht0_err = new double[1];
		double[] slope_err = new double[1];

		precise_p = 1;
		update_p = 0;
		n_repeats = 1;

		String input_file = null;
		String output_file;

		// read_poly(pg);
		poly_to_turn_rep(pg, trg);
		g = trg;

		// read_poly(pf);
		poly_to_turn_rep(pf, trf);
		f = trf;

		init_vals(f, g, ht0, slope, alpha);
		init_events(f, g);
		metric2 = h_t0min(f, g, ht0[0], slope[0], alpha[0],
				update_p == 1 ? reinit_interval(f, g) : 0, theta_star, e,
				ht0_err, slope_err);

		metric = metric2 > 0 ? Math.sqrt(metric2) : 0;
		
		/*
		System.err.println(metric + "\t" + turn(theta_star[0], 0) * 180 / Math.PI + "\t" + tr_i(f, e[0].fi) + "\t" + tr_i(g, e[0].gi)
				+ "\t" + ht0_err[0] + "\t" + slope_err[0]);
        */
		result[0] = metric;
		result[1] = turn(theta_star[0], 0) * 180 / Math.PI;
		result[2] = tr_i(f, e[0].fi);
		result[3] = tr_i(g, e[0].gi);
		result[4] = ht0_err[0];
		result[5] = slope_err[0];
		
	}
  
  
    public void test(String args[]) {
    	
    	int update_p, precise_p, n_repeats, i;
        TURN_REP trf = new TURN_REP();
        TURN_REP trg = new TURN_REP();
        TURN_REP f = new TURN_REP();
        TURN_REP g = new TURN_REP();
        poly pf = new poly();
        poly pg = new poly();
        EVENT[] e = new EVENT[1];
        e[0] = new EVENT();
        double []ht0 = new double[1];
        double []slope = new double[1];
        double []alpha = new double[1];
        double []theta_star = new double[1];
        double metric2 = 0, metric; 
        double []ht0_err = new double[1];
        double []slope_err = new double[1];
    	
        precise_p = 0;
        update_p = 1;
        n_repeats = 1;
        
        String input_file = null;
        String output_file;
        
        for ( i = 0; i < args.length; i++ ) {
        	String command = args[i];
        	if ( command.equals("-p")) {
        		precise_p = 1;
        	} else if ( command.equals("-n")) {
        		update_p = 0;
        	} else if ( command.equals("-r")) {
        		int num = Integer.valueOf(args[i+1]);
        		i++;
        		n_repeats = num;
        	} else if ( command.equals("<")) {
        		input_file = args[i+1];
        		i++;
        	} else if ( command.equals(">")) {
        		output_file = args[i+1];
        		i++;
        	}
        	
        }
        
		try {

			bufferReader = new BufferedReader(new FileReader(input_file));

			if (read_poly(pg) != 0) {
				poly_to_turn_rep(pg, trg);
				g = trg;
				while (read_poly(pf) != 0) {
					poly_to_turn_rep(pf, trf);
					f = trf;

					/* Performance measure repeat loop. */
					for (i = 0; i < n_repeats; ++i) {

						init_vals(f, g, ht0, slope, alpha);
						init_events(f, g);
						metric2 = h_t0min(f, g, ht0[0], slope[0], alpha[0],
								update_p == 1 ? reinit_interval(f, g) : 0,
								theta_star, e, ht0_err, slope_err);
					}

					/*
					 * Fixups: The value of metric2 can be a tiny negative
					 * number for an exact match. Call it 0. Theta_star can be
					 * over 360 or under -360 because we handle t=0 events at
					 * t=1. Normalize to [-PI,PI).
					 */
					metric = metric2 > 0 ? Math.sqrt(metric2) : 0;
					/*
					 * printf(precise_p ? "%.18lg %.18lg %d %d %lg %lg" :
					 * "%lg %lg %d %d %lg %lg", metric, turn(theta_star[0],
					 * 0)*180/Math.PI, tr_i(f, e.fi), tr_i(g, e.gi), ht0_err,
					 * slope_err);
					 */
					System.err.println(metric + "\t" + turn(theta_star[0], 0) * 180 / Math.PI + "\t" + 
					        tr_i(f, e[0].fi) + "\t" + tr_i(g, e[0].gi) + "\t" + ht0_err[0]
									+ "\t" + slope_err[0]);

				} // end while

			} // end if read_poly

			bufferReader.close();
		} catch ( IOException ee ) {
        	ee.printStackTrace();
        }
        
    }
    
    int read_poly(poly polygon) {
    	int line = 0;
    	int i;
    
    	i = 0;
        String inputLine;
        try {
	    	while ( !( inputLine = bufferReader.readLine()).isEmpty() ) {
	    		String [] tokens = inputLine.split(" ");
	    	
	    		polygon.pt[i] = new POINT();
	    	    polygon.pt[i].x = Double.valueOf(tokens[0]);
	    	    polygon.pt[i].y = Double.valueOf(tokens[1]);
                if ( ++i >= poly.MAX_PTS ) {
                	System.err.println("Polygon too big");
                }
	    	}
        } catch ( Exception e ) {
        	e.printStackTrace();
        }
    	
    	return (polygon.n = i);
    }
    
	
}