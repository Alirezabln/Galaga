// DE2-115 wrapper

module SpaceDash_wrapper(input  logic       CLOCK_50,
                          input  logic [10:0] SW,
                          input logic	[3:0] KEY,
                          output logic       VGA_CLK,	
                          output logic       VGA_HS,
                          output logic       VGA_VS,
                          output logic       VGA_SYNC_N,
                          output logic       VGA_BLANK_N,
                          output logic [7:0] VGA_R,
                          output logic [7:0] VGA_G,
                          output logic [7:0] VGA_B,
                          output logic [6:0] HEX0,
                          output logic [6:0] HEX1,
                          output logic [6:0] HEX2,
                          output logic [6:0] HEX3);
						

  spaceDash spaceDash1(CLOCK_50, SW[0], ~KEY[0], ~KEY[1], ~KEY[2], ~KEY[3], SW[10:1],
					    VGA_CLK, VGA_HS, VGA_VS, VGA_SYNC_N, VGA_BLANK_N,
			            VGA_R, VGA_G, VGA_B, HEX0, HEX1, HEX2, HEX3);
			 
endmodule

