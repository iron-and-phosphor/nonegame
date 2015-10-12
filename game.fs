open System.Collections.Generic 
open System 
open System.Windows.Forms 
open System.ComponentModel 
open System.Drawing 
open System.Drawing.Drawing2D 

let w = 500
let h = 500
let wc = 250
let hc = 250

let drawingform = new Form(Text="Game",AutoScaleDimensions=new System.Drawing.SizeF(60.0F, 13.0F),ClientSize=new System.Drawing.Size(w,h),StartPosition=FormStartPosition.CenterScreen) 

let mutable color=new ColorBlend() 
let gr=drawingform.CreateGraphics() 
gr.SmoothingMode<-SmoothingMode.HighQuality 

drawingform.Load.Add(fun background -> 
  gr.TranslateTransform(250.0F,250.0F)
  color.Colors<-[|Color.Indigo;Color.Red;Color.Gray;Color.Blue|]
  drawingform.BackColor<-Color.White) 

type vert = {
  X : float
  Z : float
}
type wall = {
  B : int
  E : int
}
type portal = {
  S : int
  B : int
  E : int
}
type sector = {
  F : float
  C : float
  W : int list
  P : int list
}
type player = {
  mutable PX : float
  mutable PZ : float
  mutable PA : float 
  mutable PH : float
  mutable PS : int
}
type world = {
  p1 : player
  verts : Map<int,vert>
  walls : Map<int,wall>
  portals : Map<int,portal>
  rooms : Map<int,sector>
}

let w0 = {
  p1 = {PX=0.0;PZ=0.0;PA=0.0;PH=1.0;PS=0}
  verts = Map.ofList [(0,{X= -1.0;Z=  2.0});(1,{X=  1.0;Z=  2.0});
                      (2,{X=  1.0;Z= -1.0});(3,{X= -1.0;Z= -1.0});
                      (4,{X= -1.0;Z=  3.0});(5,{X=  1.0;Z=  3.0});
                      (6,{X=  2.0;Z=  3.0});(7,{X=  2.0;Z=  2.0})]
  walls = Map.ofList [(0,{B=2;E=3});(1,{B=1;E=2});
                      (2,{B=5;E=1});(3,{B=0;E=4});
                      (4,{B=4;E=5});(5,{B=3;E=0});
                      (6,{B=5;E=6});(7,{B=6;E=7});
                      (8,{B=7;E=1})]
  portals = Map.ofList [(0,{S=1;B=0;E=1});(1,{S=0;B=1;E=0});
                        (2,{S=2;B=5;E=1});(3,{S=1;B=1;E=5})]
  rooms = Map.ofList [(0,{F=0.0;C=2.0;W=[0;1;5];P=[0]});(1,{F=0.5;C=1.75;W=[4;3];P=[2;1]});
                      (2,{F=0.0;C=2.0;W=[6;7;8];P=[3]})]
}

let rec loop = ()

let fncross x1 y1 x2 y2 = (x1*y2) - (y1*x2)
let intersect x1 y1 x2 y2 x3 y3 x4 y4 =
  let d1 = fncross x1 y1 x2 y2
  let d2 = fncross x3 y3 x4 y4
  let det = fncross (x1-x2) (y1-y2) (x3-x4) (y3-y4)
  let x = (fncross d1 (x1-x2) d2 (x3-x4)) / det
  let y = (fncross d1 (y1-y2) d2 (y3-y4)) / det
  x,y

let drawpoints x1 x2 y1 y2 y3 y4 c =
  let p1 = new Point( int(x1*float(wc)),int(y1*float(hc)))
  let p2 = new Point( int(x1*float(wc)),int(y2*float(hc)))
  let p3 = new Point( int(x2*float(wc)),int(y3*float(hc)))
  let p4 = new Point( int(x2*float(wc)),int(y4*float(hc)))
  let c1 = new Point( -wc, hc)
  let c2 = new Point(  wc, hc)
  let f1 = new Point( -wc,-hc)
  let f2 = new Point(  wc,-hc)
  let wp1= [|p1;p2;p3;p4|]
  let cp1= [|p2;p3;c2;c1|]
  let fp1= [|p1;p4;f2;f1|]
  gr.FillPolygon(new SolidBrush(color.Colors.[0]),wp1,FillMode.Winding)  
  gr.FillPolygon(new SolidBrush(color.Colors.[2]),cp1,FillMode.Winding)  
  gr.FillPolygon(new SolidBrush(color.Colors.[3]),fp1,FillMode.Winding)  



let transformz x1 x2 z1 z2 w ro =
  let x1 = x1 * 1.0 / z1
  let x2 = x2 * 1.0 / z2
  let y1 = (w.rooms.Item(ro).F - w.p1.PH) / z1
  let y2 = (w.rooms.Item(ro).C - w.p1.PH) / z1
  let y3 = (w.rooms.Item(ro).C - w.p1.PH) / z2
  let y4 = (w.rooms.Item(ro).F - w.p1.PH) / z2
  drawpoints x1 x2 y1 y2 y3 y4 0
let drawwall w ro wi = 
  if (w.verts.Item(w.walls.Item(wi).B).Z >= 0.0 || w.verts.Item(w.walls.Item(wi).E).Z >= 0.0) then
    let x1 = w.verts.Item(w.walls.Item(wi).B).X
    let x2 = w.verts.Item(w.walls.Item(wi).E).X
    let z1 = w.verts.Item(w.walls.Item(wi).B).Z
    let z2 = w.verts.Item(w.walls.Item(wi).E).Z
    let ix1,iz1 = intersect x1 z1 x2 z2 -0.0001 0.0001 1.0 -1.0
    let ix2,iz2 = intersect x1 z1 x2 z2  0.0001 0.0001 1.0  1.0 
    if z1 <= 0.0 then
      if iz1 > 0.0 then 
        transformz ix1 x2 iz1 z2 w ro
      else 
        transformz ix2 x2 iz2 z2 w ro
    elif z2 <= 0.0 then
      if iz1 > 0.0 then 
        transformz x1 ix1 z1 iz1 w ro
      else 
        transformz x1 ix2 z1 iz2 w ro
    else transformz x1 x2 z1 z2 w ro

let drawpointspo x1 x2 y1 y2 y3 y4 pf1 pf2 pc1 pc2 c =
  let p1 = new Point( int(x1*float(wc)), int(y1*float(hc)))
  let p2 = new Point( int(x1*float(wc)), int(y2*float(hc)))
  let p3 = new Point( int(x2*float(wc)), int(y3*float(hc)))
  let p4 = new Point( int(x2*float(wc)), int(y4*float(hc)))
  let pf1a= new Point(int(x1*float(wc)),int(pf1*float(hc)))
  let pf2a= new Point(int(x2*float(wc)),int(pf2*float(hc)))
  let pc1a= new Point(int(x1*float(wc)),int(pc1*float(hc)))
  let pc2a= new Point(int(x2*float(wc)),int(pc2*float(hc)))
  let c1 = new Point( -wc, hc)
  let c2 = new Point(  wc, hc)
  let f1 = new Point( -wc,-hc)
  let f2 = new Point(  wc,-hc)
  let pf1= [|p1;pf1a;pf2a;p4|]
  let pc1= [|pc1a;p2;p3;pc2a|]
  let cp1= [|p2;p3;c2;c1|]
  let fp1= [|p1;p4;f2;f1|]
  gr.FillPolygon(new SolidBrush(color.Colors.[1]),pf1,FillMode.Winding)  
  gr.FillPolygon(new SolidBrush(color.Colors.[1]),pc1,FillMode.Winding)  
  gr.FillPolygon(new SolidBrush(color.Colors.[2]),cp1,FillMode.Winding)  
  gr.FillPolygon(new SolidBrush(color.Colors.[3]),fp1,FillMode.Winding)  


let transformzpo x1 x2 z1 z2 w ro po =
  let x1 = x1 * 1.0 / z1
  let x2 = x2 * 1.0 / z2
  let y1 = (w.rooms.Item(ro).F - w.p1.PH) / z1
  let y2 = (w.rooms.Item(ro).C - w.p1.PH) / z1
  let y3 = (w.rooms.Item(ro).C - w.p1.PH) / z2
  let y4 = (w.rooms.Item(ro).F - w.p1.PH) / z2
  let pf1 = ( w.rooms.Item(w.portals.Item(po).S).F - w.p1.PH) / z1
  let pf2 = ( w.rooms.Item(w.portals.Item(po).S).F - w.p1.PH) / z2
  let pc1 = ( w.rooms.Item(w.portals.Item(po).S).C - w.p1.PH) / z1
  let pc2 = ( w.rooms.Item(w.portals.Item(po).S).C - w.p1.PH) / z2
  drawpointspo x1 x2 y1 y2 y3 y4 pf1 pf2 pc1 pc2 0

let drawportalframe w ro po =
  if (w.verts.Item(w.portals.Item(po).B).Z >= 0.0 || w.verts.Item(w.portals.Item(po).E).Z >= 0.0) then
    let x1 = w.verts.Item(w.portals.Item(po).B).X
    let x2 = w.verts.Item(w.portals.Item(po).E).X
    let z1 = w.verts.Item(w.portals.Item(po).B).Z
    let z2 = w.verts.Item(w.portals.Item(po).E).Z
    let ix1,iz1 = intersect x1 z1 x2 z2 -0.0001 0.0001 1.0 -1.0
    let ix2,iz2 = intersect x1 z1 x2 z2  0.0001 0.0001 1.0  1.0 
    if z1 <= 0.0 then
      if iz1 > 0.0 then 
        transformzpo ix1 x2 iz1 z2 w ro po
      else 
        transformzpo ix2 x2 iz2 z2 w ro po
    elif z2 <= 0.0 then
      if iz1 > 0.0 then 
        transformzpo x1 ix1 z1 iz1 w ro po
      else 
        transformzpo x1 ix2 z1 iz2 w ro po
    else transformzpo x1 x2 z1 z2 w ro po


let rec drawroom w ro =
  let rec portaldraw po =
    match po with
    | [] -> ()
    | x::xs -> 
      if (w.verts.Item(w.portals.Item(x).B).X / w.verts.Item(w.portals.Item(x).B).Z) < 
         (w.verts.Item(w.portals.Item(x).E).X / w.verts.Item(w.portals.Item(x).E).Z) then
        drawroom (w) (w.portals.Item(x).S)
        drawportalframe w ro x
        portaldraw xs
      else ()
  let portallist = w.rooms.Item(ro).P
  portaldraw portallist
  let rec walldraw wa =
    match wa with
    | [] -> ()
    | x::xs -> 
      drawwall w ro x
      walldraw xs
  let walllist = w.rooms.Item(ro).W
  walldraw walllist
  
let translateworld w = 
  let rec changevect vl = 
    match vl with
    | [] -> []
    | (x1,x2)::xs -> ((x1,{X=(x2.X-w.p1.PX);Z=(x2.Z-w.p1.PZ)})::(changevect xs))
  let wn = {
    p1 = w.p1
    verts = Map.ofList (changevect (w.verts |> Map.toList ))
              
    walls = w.walls
    portals = w.portals
    rooms = w.rooms
  }
  wn

drawingform.KeyPress.Add(fun key ->
  let dx = 0.1
  match key.KeyChar with
  | 'w' -> 
    do w0.p1.PZ <- w0.p1.PZ +  1.0 * dx
    drawroom (translateworld w0) 0
  | 's' -> 
    do w0.p1.PZ <- w0.p1.PZ + -1.0 * dx
    drawroom (translateworld w0) 0
  | 'a' -> 
    do w0.p1.PX <- w0.p1.PX + -1.0 * dx
    drawroom (translateworld w0) 0
  | 'd' -> 
    do w0.p1.PX <- w0.p1.PX +  1.0 * dx
    drawroom (translateworld w0) 0
  | _ -> drawroom w0 0

)

drawingform.MouseMove.Add(fun trail ->  
  //if (trail.Button=System.Windows.Forms.MouseButtons.Left)then
  //let test = drawwall w0 0 0 
  let p1 = new Point( int(w0.verts.Item(0).X) ,int(-w0.p1.PH))
  let p2 = new Point( int(w0.verts.Item(0).X),int(w0.p1.PH))
  let p3 = new Point( int(w0.verts.Item(1).X),int(-w0.p1.PH))
  let p4 = new Point( int(w0.verts.Item(1).X),int(w0.p1.PH))
  let cp1= [|p1;p2;p3|]
  let cp2 = [|p4;p2;p3|]
  gr.FillPolygon(new SolidBrush(color.Colors.[0]),cp1,FillMode.Winding)  
  gr.FillPolygon(new SolidBrush(color.Colors.[1]),cp2,FillMode.Winding)  
  //gr.FillRectangle(new SolidBrush(color.Colors.[0]),new Rectangle(trail.X,trail.Y,50,50))
  )  

//erasebutton.Click.Add( fun erase -> gr.Clear(Color.White))                                                                                                      
//exitbutton.Click.Add(fun quit -> drawingform.Close()) 
                                                       
Application.Run(drawingform) 