# COLORS: Color Coordinate Conversion

COLORS is a FORTRAN90 library which converts color data from one system to another.

The color coordinate systems considered include CIELAB, CIELUV, CIExyY, CIEXYZ, CMY, CMYK, HLS, HSI, HSV, HVC, LCC, NCS, PhotoYCC, RGB, Y'CbCr, Y'IQ, Y'PbPr and Y'UV.

Color specification and conversion is a suprisingly difficult subject. For a particular situation, getting more red just means increasing some number. But to accurately try to take a given color that shows up on one computer monitor screen, write down the color specification numbers and the illumination, and then figure out the corresponding color specification numbers in a different color system on a different device (a printer, a television) with a different illumination is actually an extraordinary art.

## Licensing:

The computer code and data files described and made available on this web page are distributed under the GNU LGPL license.


## Related Data and Programs:

COLOR, a dataset directory which describes colors by name, triples of integers between 0 and 255, or triples of real numbers between 0 and 1.


## Reference:

- George Agoston, Color Theory and Its Application in Art and Design, Second edition, Springer, 1987, ISBN: 038709654X, LC: QC495.A32.
- Wayne Brown, Barry Shepherd, Graphics File Formats, Manning Publications, 1995, ISBN: 1884777007, LC: T385.B777.
- James Foley, Andries vanDam, Steven Feiner, John Hughes, Computer Graphics, Principles and Practice, Second Edition, Addison Wesley, 1995, ISBN: 0201848406, LC: T385.C5735.
- Brand Fortner, Number by Colors, A Guide to Using Color to Understand Technical Data, Springer, 1997, ISBN: 0387946853, LC: QC495.F6.
- Edward Giorgianni, Thomas Madden, Digital Color Management, Encoding Solutions, Addison Wesley, 1998, ISBN: 0201634260, LC: TA1637.G56.
- Jonas Gomes, Luiz Velho, Image Processing for Computer Graphics, Springer, 1997, ISBN: 0387948546, LC: T385.G65.
- Roy Hall, Illumination and Color in Computer Generated Imagery, Springer, 1989, ISBN: 0387967745, LC: T385.H327.
- International Electrotechnical Commission, The sRGB standard, Standard IEC 61966-2-1, http://www.srgb.com
- Deane Judd, Gunter Wyszecki, Color in Business, Science, and Industry, Wiley, 1975, ISBN: 0471452122, LC: QC495.J79.
- Olof Kylander, Karin Kylander, GIMP: The Official Handbook, Coriolis Open Press, 1999, ISBN: 1576105202, LC: T385.K866.
- David Martindale, Alan Paeth, Television Color Encoding and Hot Broadcast Colors, in Graphics Gems II, edited by James Arvo, Academic Press, 1991, ISBN: 0120644819, LC: T385.G6972.
- Calvin McCamy, H Marcus, JG Davidson, A Color Rendition Chart, Journal of Applied Photographic Engineering, Volume 11, Number 3, 1976, pages 95-99.
- Guenter Wyszecki, Walter Stiles, Color Science: Concepts and Methods, Quantitative Data and Formulas, John Wiley, 1967, ISBN: 0-471-02106-7, LC: QC495.W88.


## Source Code:

- colors.f90, the source code;
- colors.sh, commands to compile the source code;
- colors.txt, a color name data file with the R, G, B values are integers between 0 and 255.
- colors_real.txt, a color name data file with the R, G, B, values as real numbers between 0.0 and 1.0.


## Examples and Tests:

- colors_prb.f90, the calling program;
- colors_prb.sh, commands to compile, link and run the calling program;
- colors_prb_output.txt, the output file.


## List of Routines:

- ANGLE_TO_RGB returns a color on the perimeter of the color hexagon.
- ATAN4 computes the inverse tangent of the ratio Y / X.
- CH_CAP capitalizes a single character.
- CHART_XYZ_CAP returns the CIE XYZ values of a 24 box color chart.
- CMY_CHECK corrects out-of-range CMY color coordinates.
- CMY_TO_CMYK converts CMY to CMYK color coordinates.
- CMY_TO_RGB converts CMY to RGB color coordinates.
- CMYK_CHECK corrects out-of-range CMYK color coordinates.
- CMYK_TO_CMY converts CMYK to CMY color coordinates.
- CMYK_TO_RGB converts CMYK to RGB color coordinates.
- GET_SEED returns a seed for the random number generator.
- GET_UNIT returns a free FORTRAN unit number.
- GRAYSCALE_LUV returns a grayscale in the CIE LUV system.
- GRAYSCALE_RGB returns a grayscale in the RGB system.
- HLS_CHECK corrects out-of-range HLS color coordinates.
- HLS_TO_RGB converts HLS to RGB color coordinates.
- HLS_VALUE is a utility function used by HLS_TO_RGB.
- HSI_TO_RGB converts HSI to RGB color coordinates.
- HSV_CHECK corrects out-of-range HSV color coordinates.
- HSV_TO_RGB converts HSV to RGB color coordinates.
- HVC_CHECK corrects out-of-range HVC color coordinates.
- I4_LOG_2 returns the integer part of the logarithm base 2 of |I|.
- I4_TO_ANGLE maps integers to points on a circle.
- I4_UNIFORM returns a scaled pseudorandom I4.
- INTERP does simple linear interpolation in a table.
- LAB_CHECK corrects out-of-range CIE LAB color coordinates.
- LAB_PROP returns certain properties of a CIE LAB color.
- LAB_TO_XYZ_CAP converts CIE LAB to CIE XYZ color coordinates.
- LCC_TO_RGBPRIME converts LCC to R'G'B' color coordinates.
- LCC_TO_YCBCR converts LCC to Y'CbCr color coordinates.
- LCC_TO_YCC converts LCC to PhotoYCC color coordinates.
- LIN_TO_NONLIN converts a linear light intensity to nonlinear video signal.
- LUV_CHECK corrects out-of-range CIE LUV color coordinates.
- LUV_PROP returns certain properties of a CIE LUV color.
- LUV_TO_XYZ_CAP converts CIE LUV to CIE XYZ color coordinates.
- NAME_TEST supplies color names for tests.
- NAME_TO_PRIMARIES returns CIE xy chromaticities of television primaries.
- NAME_TO_RGB converts a string to RGB colors.
- NAME_TO_XYZ converts a color or illuminant name to CIE xyz color coordinates.
- NCS_CHECK corrects out-of-range NCS color coordinates.
- NCS_TO_RGB converts NCS to RGB color coordinates.
- NM_TO_RGBCIE converts a pure light wavelength to CIE RGB color coordinates.
- NM_TO_XYZ converts a pure light wavelength to CIE xyz color coordinates.
- NM_TO_XYZ_CAP converts a pure light wavelength to CIE XYZ color coordinates.
- NONLIN_TO_LIN converts a nonlinear video signal to a linear light intensity.
- PRIMARIES_TO_Y computes the luminance function for given primaries.
- R8_CUBERT returns the cube root of an R8.
- R8_MODP returns the nonnegative remainder of real division.
- R8_SWAP switches two R8's.
- R8_UNIFORM_01 returns a unit pseudorandom R8.
- R8MAT_SOLVE uses Gauss-Jordan elimination to solve an N by N linear system.
- RADIANS_TO_DEGREES converts an angle from radians to degrees.
- RGB709_TO_XYZ_CAP converts RGB709 to CIE XYZ color coordinates.
- RGB_CHECK corrects out-of-range RGB color coordinates.
- RGB_UNIFORM returns a random RGB color.
- RGB_NAMED_UNIFORM returns a random named RGB color.
- RGB_TEST supplies RGB values for tests.
- RGB_TO_CMY converts RGB to CMY color coordinates.
- RGB_TO_CMYK converts RGB to CMYK color coordinates.
- RGB_TO_HLS converts RGB to HLS color coordinates.
- RGB_TO_HSI converts RGB to HSI color coordinates.
- RGB_TO_HSV converts RGB to HSV color coordinates.
- RGB_TO_HUE converts (R,G,B) colors to a hue value between 0 and 1.
- RGB_TO_NAME converts RGB colors to the name of the nearest color.
- RGB_TO_NCS converts RGB to NCS color coordinates.
- RGB_TO_RGBPRIME converts RGB to R'G'B' color coordinates.
- RGB_TO_YCBCR converts RGB to Y'CbCr color coordinates.
- RGB_TO_YIQ converts RGB to Y'IQ color coordinates.
- RGB_TO_YPBPR converts RGB to Y'PbPr color coordinates.
- RGB_TO_YUV converts RGB to Y'UV color coordinates.
- RGBCIE_TO_XYZ_CAP converts CIE RGB to CIE XYZ color coordinates.
- RGBPRIME_TO_LCC converts R'G'B' to LCC color coordinates.
- RGBPRIME_TO_RGB converts R'G'B' to RGB color coordinates.
- S_C_DELETE removes all occurrences of a character from a string.
- S_EQI is a case insensitive comparison of two strings for equality.
- SRGB_TO_XYZ_CAP converts sRGB to CIE XYZ color coordinates.
- T_TO_SPD evaluates the black body power spectrum at a given temperature.
- T_TO_XY returns CIE xyz color coordinates for black body radiation.
- TIMESTAMP prints the current YMDHMS date as a time stamp.
- UVPRIME_TO_XYZ converts CIE u'v' to CIE xyz color coordinates.
- UVPRIMEY_TO_XYZ_CAP converts CIE u'v'Y to CIE XYZ color coordinates.
- XY_TO_UVWPRIME converts CIE xy to CIE u'v'w' color coordinates.
- XYY_CHECK corrects out-of-range CIE xyY color coordinates.
- XYY_TO_XYZ_CAP converts CIE xyY to CIE XYZ color coordinates.
- XYZ_CAP_TO_LAB converts CIE XYZ to CIE LAB color coordinates.
- XYZ_CAP_TO_LUV converts CIE XYZ to CIE LUV color coordinates.
- XYZ_CAP_TO_RGB709 converts CIE XYZ to RGB709 color coordinates.
- XYZ_CAP_TO_RGBCIE converts CIE XYZ to CIE RGB color coordinates.
- XYZ_CAP_TO_SRGB converts CIE XYZ to sRGB color coordinates.
- XYZ_CAP_TO_UVWPRIME converts CIE XYZ to CIE u'v'w' color coordinates.
- XYZ_CAP_TO_XYY converts CIE XYZ to CIE xyY color coordinates.
- XYZ_CAP_TO_YCC converts CIE XYZ to PhotoYCC color coordinates.
- XYZCAP_CHECK corrects out-of-range CIE XYZ color coordinates.
- YCBCR_CHECK corrects out-of-range Y'CbCr color coordinates.
- YCBCR_TO_LCC converts Y'CbCr to LCC color coordinates.
- YCBCR_TO_RGB converts Y'CbCr to RGB color coordinates.
- YCBCR_TO_YCC converts Y'CbCr to PhotoYCC color coordinates.
- YCC_TEST supplies PhotoYCC values for tests.
- YCC_TO_LCC converts PhotoYCC to LCC color coordinates.
- YCC_TO_XYZ_CAP converts PhotoYCC to CIE XYZ color coordinates.
- YCC_TO_YCBCR converts PhotoYCC to Y'CbCr color coordinates.
- YIQ_CHECK corrects out-of-range Y'IQ color coordinates.
- YIQ_TO_RGB converts Y'IQ to RGB color coordinates.
- YIQ_TO_YUV converts Y'IQ to Y'UV color coordinates.
- YPBPR_TO_RGB converts Y'PbPr to RGB color coordinates.
- YUV_CHECK corrects out-of-range Y'UV color coordinates.
- YUV_TO_RGB converts Y'UV to RGB color coordinates.
- YUV_TO_YIQ converts Y'UV to Y'IQ color coordinates.

You can go up one level to the FORTRAN90 source codes.

Last revised on 31 December 2010.
