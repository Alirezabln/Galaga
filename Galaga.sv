// vga.sv

module vga(input  logic clk, reset,
           input  logic keyright, keyleft, keyup, keydown,
           output logic vgaclk,          // 25.175 MHz VGA clock 
           output logic hsync, vsync, 
           output logic sync_b, blank_b, // to monitor & DAC 
           output logic [7:0] r, g, b);  // to video DAC 

  logic [9:0] x, y, horizontalMove, verticalMove;

// Horizontal Movement
  always_ff @(posedge vsync, posedge reset) begin
		if (reset) 
			horizontalMove <= 0;
		else if ((keyright) && (horizontalMove+10'd325 < 10'd620))
			horizontalMove <= horizontalMove + 1;
		else if ((keyleft) && (horizontalMove+10'd325 > 10'd11))
			horizontalMove <= horizontalMove - 1;
		else
			horizontalMove <= horizontalMove;
	end
	
	// Vertical Movement
	always_ff @(posedge vsync, posedge reset) begin
		if (reset)
			verticalMove <= 0;
		else if ((keyup) && (verticalMove+10'd460 <= 10'd460)) 
			verticalMove <= verticalMove + 1;
		else if ((keydown) && (verticalMove +10'd460 >= 10'd30))
			verticalMove <= verticalMove - 1;
		else
			verticalMove <= verticalMove;
	end
  
  // divide 50 MHz input clock by 2 to get 25 MHz clock
  always_ff @(posedge clk, posedge reset)
    if (reset)
	   vgaclk = 1'b0;
    else
	   vgaclk = ~vgaclk;
		
  // generate monitor timing signals 
  vgaController vgaCont(vgaclk, reset, hsync, vsync, sync_b, blank_b, x, y); 

  // user-defined module to determine pixel color 
  videoGen videoGen(x, y, horizontalMove, verticalMove, vsync, reset, r, g, b);
  
endmodule 


module vgaController #(parameter HBP     = 10'd48,   // horizontal back porch
                                 HACTIVE = 10'd640,  // number of pixels per line
                                 HFP     = 10'd16,   // horizontal front porch
                                 HSYN    = 10'd96,   // horizontal sync pulse = 60 to move electron gun back to left
                                 HMAX    = HBP + HACTIVE + HFP + HSYN, //48+640+16+96=800: number of horizontal pixels (i.e., clock cycles)
                                 VBP     = 10'd32,   // vertical back porch
                                 VACTIVE = 10'd480,  // number of lines
                                 VFP     = 10'd11,   // vertical front porch
                                 VSYN    = 10'd2,    // vertical sync pulse = 2 to move electron gun back to top
                                 VMAX    = VBP + VACTIVE + VFP  + VSYN) //32+480+11+2=525: number of vertical pixels (i.e., clock cycles)                      

     (input  logic vgaclk, reset,
      output logic hsync, vsync, sync_b, blank_b, 
      output logic [9:0] hcnt, vcnt); 

      // counters for horizontal and vertical positions 
      always @(posedge vgaclk, posedge reset) begin 
        if (reset) begin
          hcnt <= 0;
          vcnt <= 0;
        end
        else  begin
          hcnt++; 
      	   if (hcnt == HMAX) begin 
            hcnt <= 0; 
  	        vcnt++; 
  	        if (vcnt == VMAX) 
  	          vcnt <= 0; 
          end 
        end
      end 
	  

      // compute sync signals (active low) 
      assign hsync  = ~( (hcnt >= (HACTIVE + HFP)) & (hcnt < (HACTIVE + HFP + HSYN)) ); 
      assign vsync  = ~( (vcnt >= (VACTIVE + VFP)) & (vcnt < (VACTIVE + VFP + VSYN)) ); 
      // assign sync_b = hsync & vsync; 
      assign sync_b = 1'b0;  // this should be 0 for newer monitors

      // force outputs to black when not writing pixels
      // The following also works: assign blank_b = hsync & vsync; 
      assign blank_b = (hcnt < HACTIVE) & (vcnt < VACTIVE); 
endmodule 


module videoGen(input logic [9:0] x, y, input logic vsync, reset, output logic [7:0] r, g, b); 
  logic rpixel;

  rocket r1(x, y, vsync, reset, rpixel);
  
  assign {r, g, b} = rpixel ? 24'hFFFFFF : 24'h000000; // white color

endmodule

// display the rocket at the bottom middle of the screen
module rocket(input logic [9:0] x, y, horizontalMove, verticalMove,
				input logic vsync,
				input logic reset,
				output logic rpixel);
	
    // Data Structure for Rocket Shape
    logic [7:0][10:0] rocket_shape = {
      11'b00011111000,
      11'b11111111111,
      11'b00111111100,
      11'b00011111000,
      11'b00001110000,
      11'b00001110000,
      11'b00001110000,
      11'b00000100000
    };

	 logic [8:0] xleft, xright, yleft, yright;
	 assign xleft = 9'd315;
	 assign xright = 9'd325;
	 assign yleft = 9'd460;
	 assign yright = 9'd468;

  // Inside rocket module, assuming rocket starts at X=315, Y=460
    always_comb begin
        if ((x-horizontalMove >= xleft) && (x-horizontalMove <= xright) && 
            (y-verticalMove >= yleft) && (y-verticalMove < yright) && 
            rocket_shape[y-yleft-verticalMove][x-xleft-horizontalMove])  begin
                rpixel = 1; // White
            end
        else begin
            rpixel = 0; // Black
        end
    end 


endmodule





