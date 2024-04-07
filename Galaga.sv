// vga.sv

module vga(input  logic clk, reset,
           input  logic keyright, keyleft,
           output logic vgaclk,          // 25.175 MHz VGA clock 
           output logic hsync, vsync, 
           output logic sync_b, blank_b, // to monitor & DAC 
           output logic [7:0] r, g, b);  // to video DAC 

  logic [9:0] x, y; 
  // logic [9:0] horizontalMove;
  // always_ff @(posedge vsync, posedge reset) begin
	// if (reset) begin
	   // horizontalMove <= 0;
       // horizontalMove < = 0;
    // end
	// else if (keyright)
	   // horizontalMove <= horizontalMove + 1;
	// else if (keyleft)
	   // horizontalMove <= horizontalMove - 1;
	// else
	   // horizontalMove <= horizontalMove;
  // end

  // Use a clock divider to create the 25 MHz VGA pixel clock 
  // 25 MHz clk period = 40 ns 
  // Screen is 800 clocks wide by 525 tall, but only 640 x 480 used for display 
  // HSync = 1/(40 ns * 800) = 31.25 kHz 
  // Vsync = 31.25 KHz / 525 = 59.52 Hz (~60 Hz refresh rate) 
  
  // divide 50 MHz input clock by 2 to get 25 MHz clock
  always_ff @(posedge clk, posedge reset)
    if (reset)
	   vgaclk = 1'b0;
    else
	   vgaclk = ~vgaclk;
		
  // generate monitor timing signals 
  vgaController vgaCont(vgaclk, reset, hsync, vsync, sync_b, blank_b, x, y); 

  // user-defined module to determine pixel color 
  videoGen videoGen(x, y,vsync, reset, r, g, b);
  
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
module rocket(input logic [9:0] x, y,
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

  // Inside rocket module, assuming rocket starts at X=315, Y=460
    always_comb begin
        if ((x >= 315) && (x <= 325) && 
            (y >= 460) && (y < 468) && 
            rocket_shape[y-460][x-315])  begin
                rpixel = 1; // White
            end
        else begin
            rpixel = 0; // Black
        end
    end 

endmodule





