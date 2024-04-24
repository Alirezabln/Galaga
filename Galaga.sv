// vga.sv

module vga(input  logic clk, reset,
           input  logic keyright, keyleft, keyup, keydown, 
           input  logic [9:0] seed,
           output logic vgaclk,          // 25.175 MHz VGA clock 
           output logic hsync, vsync, 
           output logic sync_b, blank_b, // to monitor & DAC 
           output logic [7:0] r, g, b);  // to video DAC 

  logic [9:0] x, y;
	
  // divide 50 MHz input clock by 2 to get 25 MHz clock
  always_ff @(posedge clk, posedge reset)
    if (reset)
	   vgaclk = 1'b0;
    else
	   vgaclk = ~vgaclk;
		
  // generate monitor timing signals 
  vgaController vgaCont(vgaclk, reset, hsync, vsync, sync_b, blank_b, x, y); 

  // user-defined module to determine pixel color 
  videoGen videoGen(x, y, seed, keyright, keyleft, keyup, keydown, vsync, reset, r, g, b);
  
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


module videoGen(input logic [9:0] x, y, seed,
					input logic keyright, keyleft, keyup, keydown, vsync, reset,
					output logic [7:0] r, g, b); 
  logic rpixel, a1pixel, a2pixel, a3pixel, a4pixel, a5pixel, a6pixel, spawnclk;
  logic [5:0] en;
  logic [9:0] RNGPos;

   lfsr lfsr1(vsync, reset, seed, RNGPos);
   clock clock1(reset, vsync, spawnclk);
   asteoridGenerator asteoridGenerator1(reset, spawnclk, en);

  rocket r1(x, y, vsync, reset, keyright, keyleft, keyup, keydown, rpixel);
  asteroid a1(x, y, RNGPos, reset, vsync, en[0], a1pixel);
  asteroid a2(x, y, RNGPos, reset, vsync, en[1], a2pixel);
  asteroid a3(x, y, RNGPos, reset, vsync, en[2], a3pixel);
  asteroid a4(x, y, RNGPos, reset, vsync, en[3], a4pixel);
  asteroid a5(x, y, RNGPos, reset, vsync, en[4], a5pixel);
  asteroid a6(x, y, RNGPos, reset, vsync, en[5], a6pixel);

  
  always_comb begin
   if (rpixel) 
			{r, g, b} = 24'hFFFFFF;
   else if ((a1pixel)|(a2pixel)|(a3pixel)|(a4pixel)|(a5pixel)|(a6pixel)) 
			{r, g, b} = 24'h8968CD;
   else
			{r, g, b} = 24'h000000;
	end

endmodule

// display the rocket at the bottom middle of the screen
module rocket(input logic [9:0] x, y,
					input logic vsync, reset, keyright, keyleft, keyup, keydown,
				 output logic rpixel);
	
    // Data Structure for Rocket Shape
    logic [15:0][14:0] rocket_shape = {
	  15'b100000010000001,
      15'b110000010000011,
	  15'b111000111000111,
      15'b111110111011111,
	  15'b101111111111101,
      15'b100111010111001,
	  15'b000111000111000,
      15'b000111101111000,
      15'b000101111101000,
      15'b000000111000000,
	  15'b000000111000000,
      15'b000000111000000,
	  15'b000000111000000,
	  15'b000000111000000,
	  15'b000000010000000,
	  15'b000000010000000,
	  15'b000000010000000
    };

	 
	 logic [9:0] xleft, xright, ytop, ybottom, horizontalMove, verticalMove;
	 
	 // Horizontal Movement
  always_ff @(posedge vsync, posedge reset) begin
		if (reset) 
			horizontalMove <= 0;
		else if ((keyright) && (horizontalMove+10'd325 < 10'd633))
			horizontalMove <= horizontalMove + 10'd1;
		else if ((keyleft) && (horizontalMove+10'd325 > 10'd18))
			horizontalMove <= horizontalMove - 10'd1;
		else
			horizontalMove <= horizontalMove;
	end
	
	// Vertical Movement
	always_ff @(posedge vsync, posedge reset) begin
		if (reset)
			verticalMove <= 0;
		else if ((keyup) && (verticalMove+10'd460 <= 10'd460)) 
			verticalMove <= verticalMove + 10'd1;
		else if ((keydown) && (verticalMove +10'd460 >= 10'd30))
			verticalMove <= verticalMove - 10'd1;
		else
			verticalMove <= verticalMove;
	end
	
	 assign xleft = 10'd312;
	 assign xright = 10'd326;
	 assign ytop = 10'd452;
	 assign ybottom = 10'd468;

  // Inside rocket module, assuming rocket starts at X=315, Y=460
    always_comb begin
        if ((x-horizontalMove >= xleft) && (x-horizontalMove <= xright) && 
            (y-verticalMove >= ytop) && (y-verticalMove < ybottom) && 
            (rocket_shape[y-ytop-verticalMove][x-xleft-horizontalMove]))  begin
                rpixel = 1; // White
            end
        else begin
            rpixel = 0; // Black
        end
    end 
endmodule
	 
module asteroid(input logic [9:0] x, y, RNGPos,
						input logic reset, vsync, en, 
                output logic apixel); 

// Data Structure for Asteroid Shape
logic [19:0][29:0] asteroid_shape = {
      30'b000000000000011100000000000000,
      30'b000000000000011100000000000000,
      30'b000000000011111111111000000000,
      30'b000000000011111111111000000000,
      30'b000000000011111111111111000000,
      30'b000000111111111111111111000000,
      30'b000000111111111111111111110000,
      30'b000000111111111111111111110000,
      30'b001111111111111111111111111000,
      30'b111111111111111111111111111111,
	  30'b111111111111111111111111111111,
      30'b001111111111111111111111111100,
      30'b111111111111111111111111111111,
      30'b000111111111111111111111111000,
      30'b000000011111111111111110000000,
      30'b000000011111111111111110000000,
	  30'b000000011111111111111110000000,
	  30'b000000000011111111111000000000,
	  30'b000000000011111111111000000000,
	  30'b000000000000000111111000000000
};
	
	 logic [9:0] xleft, xright, ytop, ybottom, moveDown, initialP;
	 
     // Asteroid moving down
	always_ff @(posedge vsync, posedge reset) begin
	if (reset) begin
		moveDown <= 0;
		initialP <= RNGPos;
	end
	else if (~en) begin
		moveDown <= 0;
		initialP <= RNGPos;
		end
    else if (moveDown >= 10'd480) begin
        moveDown <= 0;
		initialP <= RNGPos;
	end
	else begin
		moveDown <= moveDown + 10'd1;
		initialP <= initialP;
    end
	end

	 assign xleft = initialP;
	 assign xright = initialP + 10'd30;
	 assign ytop = 10'd0;
	 assign ybottom = 10'd20;


  // Inside asteroid module, assuming asteroid starts at X=310, Y=0
  always_comb begin
	if ((en) && (x >= xleft) && (x < xright) &&
        (y-moveDown+10'd470 >= ytop+10'd470) && (y-moveDown+10'd470 < ybottom+10'd470) &&
        (asteroid_shape[y-ytop-moveDown][x-xleft])) begin
	  apixel = 1; 
	end
	else begin
	  apixel = 0;
	end
  end
endmodule

module lfsr(input logic clk, reset,
				input logic [9:0] seed,
				output logic [9:0] RNG);		
	logic [9:0] count;
	
	always_ff @(posedge clk, posedge reset) begin
		if (reset)	begin		count <= seed;
		end
		else 					count <= {count[0] ^ count[9], count[9:1]};
	end 

	assign RNG = count % 10'd641;

endmodule

module asteoridGenerator(input logic reset, clk, 
						output logic [5:0] en);
	logic [5:0] count;

always_ff @(posedge clk, posedge reset) begin
	if (reset) begin
		count <= 6'b000000;
	end
	else if (count == 6'b111111) begin
		count <= count;
	end
	else begin
		count <= (count << 1) + 6'b000001;
	end
	end

	assign en = count;

endmodule

module clock(input logic reset, vsync,
				output logic clk);
	logic [7:0] count;

	always_ff @(posedge reset, posedge vsync) begin
		if (reset) begin
			count <= 8'd0;
		end
		else begin
			count <= count + 8'd1;
		end
	end

	assign clk = count[7];

endmodule
