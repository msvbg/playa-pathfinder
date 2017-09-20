/* Not the prettiest code, but it works, right? */
module Math = {
  let pi = [%bs.raw "Math.PI"];
  let round x => int_of_float (floor (x +. 0.5));
};

let lerp a b x => a *. (1.0 -. x) +. b *. x;

let clamp a b x => x < a ? a : x > b ? b : x;

let drawCircle ctx (x, y) radius => {
  Canvas.Ctx.beginPath ctx;
  Canvas.Ctx.setFillStyle ctx "#fff";
  Canvas.Ctx.setStrokeStyle ctx "#000";
  Canvas.Ctx.arc ctx x y radius 0.0 (Math.pi *. 2.0);
  Canvas.Ctx.fill ctx;
  Canvas.Ctx.stroke ctx
};

let indexOf a x => {
  let rec indexOfN a x n =>
    if (a.(n) == x) {
      n
    } else {
      indexOfN a x (n + 1)
    };
  indexOfN a x 0
};

let arcStreets = [|
  "Esplanade",
  " ",
  "Awe",
  "Breath",
  "Ceremony",
  "Dance",
  "Eulogy",
  "Fire",
  "Genuflect",
  "Hallowed",
  "Inspirit",
  "Juju",
  "Kundalini",
  "Lustrate"
|];

let radialStreets = [|
  "10:00",
  "9:45",
  "9:30",
  "9:15",
  "9:00",
  "8:45",
  "8:30",
  "8:15",
  "8:00",
  "7:45",
  "7:30",
  "7:15",
  "7:00",
  "6:45",
  "6:30",
  "6:15",
  "6:00",
  "5:45",
  "5:30",
  "5:15",
  "5:00",
  "4:45",
  "4:30",
  "4:15",
  "4:00",
  "3:45",
  "3:30",
  "3:15",
  "3:00",
  "2:45",
  "2:30",
  "2:15",
  "2:00"
|];

let initialize () => {
  let (width, height) = (800.0, 800.0);
  let outerRadius = width *. 0.4;
  let innerRadius = width *. 0.2;
  let (originX, originY) = (width /. 2.0, height /. 2.0);
  let canvas = Canvas.createOnBody (int_of_float width) (int_of_float height);
  let ctx = Canvas.getContext canvas;
  let streetsToCoordinates arc radial => {
    let arc = arc == " " ? "Esplanade" : arc;
    let (arcIndex, radialIndex) = (indexOf arcStreets arc, indexOf radialStreets radial);
    let radius =
      float_of_int arcIndex /. float_of_int (Array.length arcStreets - 1) *. (
        outerRadius -. innerRadius
      ) +. innerRadius;
    let theta = Math.pi *. (((-20.0) -. float_of_int radialIndex) /. 24.0);
    (originX +. radius *. cos theta, originY +. radius *. sin theta)
  };
  Canvas.Ctx.setLineWidth ctx 1.0;
  Canvas.Ctx.setStrokeStyle ctx "#000";
  Canvas.Ctx.setTextAlign ctx "center";
  Canvas.Ctx.setTextBaseline ctx "middle";
  Canvas.Ctx.setFont ctx "14px Arial";
  let cursorX = ref 0.0;
  let cursorY = ref 0.0;
  let cursorArcStreet = ref "";
  let cursorRadialStreet = ref "";
  let sourceArcStreet = ref "Juju";
  let sourceRadialStreet = ref "7:00";
  let dijkstraMemo = Hashtbl.create 1;
  let drawArcStreets () =>
    Array.iteri
      (
        fun index street => {
          let radius =
            lerp
              innerRadius
              outerRadius
              (float_of_int index /. float_of_int (Array.length arcStreets - 1));
          let startAngle = Math.pi *. ((-4.0) /. 24.0);
          let endAngle = Math.pi *. ((-20.0) /. 24.0);
          if (street != " ") {
            if (index == 7) {
              let angleStep = (endAngle -. startAngle) /. 8.0;
              Canvas.Ctx.beginPath ctx;
              Canvas.Ctx.arc
                ctx
                originX
                originY
                radius
                (startAngle -. 1.0 *. angleStep)
                (startAngle -. 3.0 *. angleStep);
              Canvas.Ctx.stroke ctx;
              Canvas.Ctx.beginPath ctx;
              Canvas.Ctx.arc
                ctx
                originX
                originY
                radius
                (startAngle -. 4.0 *. angleStep)
                (startAngle -. 6.0 *. angleStep);
              Canvas.Ctx.stroke ctx;
              Canvas.Ctx.beginPath ctx;
              Canvas.Ctx.arc
                ctx
                originX
                originY
                radius
                (startAngle -. 10.0 *. angleStep)
                (startAngle -. 12.0 *. angleStep);
              Canvas.Ctx.stroke ctx;
              Canvas.Ctx.beginPath ctx;
              Canvas.Ctx.arc
                ctx
                originX
                originY
                radius
                (startAngle -. 14.0 *. angleStep)
                (startAngle -. 16.0 *. angleStep);
              Canvas.Ctx.stroke ctx
            } else {
              Canvas.Ctx.beginPath ctx;
              Canvas.Ctx.arc ctx originX originY radius startAngle endAngle;
              Canvas.Ctx.stroke ctx
            };
            Canvas.Ctx.fillText
              ctx
              (String.sub street 0 1)
              (originX -. 7.0 +. radius *. cos startAngle)
              (originY -. 15.0 +. radius *. sin startAngle)
          }
        }
      )
      arcStreets;
  let drawRadialStreets () =>
    Array.iteri
      (
        fun index street => {
          let angle = Math.pi *. (((-20.0) -. float_of_int index) /. 24.0);
          Canvas.Ctx.setLineWidth ctx 1.0;
          let halfLines = index mod 2 == 1;
          let innerRadius = halfLines ? innerRadius *. 1.62 : innerRadius;
          Canvas.Ctx.beginPath ctx;
          Canvas.Ctx.line
            ctx
            (originX +. innerRadius *. cos angle, originY +. innerRadius *. sin angle)
            (originX +. outerRadius *. cos angle, originY +. outerRadius *. sin angle);
          Canvas.Ctx.stroke ctx;
          Canvas.Ctx.fillText
            ctx
            street
            (originX +. 1.1 *. outerRadius *. cos angle)
            (originY +. 1.1 *. outerRadius *. sin angle)
        }
      )
      radialStreets;
  let eventToStreetIndices event => {
    let rect = Canvas.getBoundingClientRect canvas;
    let (x, y) = (event##clientX -. rect##left -. originX, event##clientY -. rect##top -. originY);
    let theta = atan2 x y;
    let normalizedAngle =
      clamp 0.0 1.0 ((theta -. Math.pi *. ((-16.0) /. 24.0)) /. (Math.pi *. (32.0 /. 24.0)));
    let radialStreetIndex =
      Math.round (normalizedAngle *. float_of_int (Array.length radialStreets - 1));
    let normalizedRadius =
      clamp
        0.0 1.0 ((sqrt (x *\* 2.0 +. y *\* 2.0) -. innerRadius) /. (outerRadius -. innerRadius));
    let arcStreetIndex =
      Math.round (normalizedRadius *. float_of_int (Array.length arcStreets - 1));
    (arcStreetIndex, radialStreetIndex)
  };
  let buildGraph () => {
    let radialStep from clockwise => radialStreets.(indexOf radialStreets from + (
                                                      clockwise ? (-1) : 1
                                                    ));
    let arcwiseStep from inwards => arcStreets.(indexOf arcStreets from + (inwards ? (-1) : 1));
    let arcLength arcIndex => {
      let normalizedIndex = float_of_int arcIndex /. float_of_int (Array.length arcStreets - 1);
      let radius = lerp innerRadius outerRadius normalizedIndex;
      Math.round (radius *. Math.pi *. 1.0 /. 24.0)
    };
    let radialLength =
      Math.round ((outerRadius -. innerRadius) /. float_of_int (Array.length arcStreets - 1));
    let euclideanDist (x1, y1) (x2, y2) =>
      Math.round (sqrt ((x1 -. x2) *\* 2.0 +. (y1 -. y2) *\* 2.0));
    List.flatten (
      List.mapi
        (
          fun arcIndex arcStreet =>
            List.flatten (
              List.map
                (
                  fun radialStreet => {
                    let point = (arcStreet, radialStreet);
                    List.flatten [
                      if (radialStreet != "10:00") {
                        let neighbor = (arcStreet, radialStep radialStreet true);
                        [((point, neighbor), arcLength arcIndex)]
                      } else {
                        []
                      },
                      if (radialStreet != "2:00") {
                        let neighbor = (arcStreet, radialStep radialStreet false);
                        [((point, neighbor), arcLength arcIndex)]
                      } else {
                        []
                      },
                      if (arcStreet != "Esplanade") {
                        let neighbor = (arcwiseStep arcStreet true, radialStreet);
                        [((point, neighbor), radialLength)]
                      } else {
                        List.map
                          (
                            fun street => {
                              let neighbor = ("Esplanade", street);
                              (
                                (point, neighbor),
                                euclideanDist
                                  (streetsToCoordinates arcStreet radialStreet)
                                  (streetsToCoordinates "Esplanade" street)
                              )
                            }
                          )
                          (Array.to_list radialStreets)
                      },
                      if (arcStreet != "Lustrate") {
                        let neighbor = (arcwiseStep arcStreet false, radialStreet);
                        [((point, neighbor), radialLength)]
                      } else {
                        []
                      }
                    ]
                  }
                )
                (Array.to_list radialStreets)
            )
        )
        (Array.to_list arcStreets)
    )
  };
  let graph = buildGraph ();
  let drawPath path =>
    switch path {
    | [] => ()
    | [(arc, radial), ...tail] =>
      Canvas.Ctx.setStrokeStyle ctx "#f00";
      Canvas.Ctx.beginPath ctx;
      let (startX, startY) = streetsToCoordinates arc radial;
      Canvas.Ctx.moveTo ctx startX startY;
      List.iter
        (
          fun (arc, radial) => {
            let (x, y) = streetsToCoordinates arc radial;
            Canvas.Ctx.lineTo ctx x y
          }
        )
        tail;
      Canvas.Ctx.setStrokeWidth ctx 2.0;
      Canvas.Ctx.stroke ctx
    };
  let path = ref [];
  let rec render () => {
    Canvas.Ctx.setFillStyle ctx "#fff";
    Canvas.Ctx.fillRect ctx 0.0 0.0 width height;
    Canvas.Ctx.setFillStyle ctx "#000";
    Canvas.Ctx.setStrokeStyle ctx "#000";
    Canvas.Ctx.setStrokeWidth ctx 1.0;
    drawArcStreets ();
    drawRadialStreets ();
    /* The man */
    drawCircle ctx (originX, originY) 25.0;
    /* Center camp */
    let (centerCampX, centerCampY) = streetsToCoordinates "Awe" "6:00";
    drawCircle ctx (centerCampX, centerCampY -. 9.0) 46.0;
    /* Plazas */
    drawCircle ctx (streetsToCoordinates "Genuflect" "9:00") 6.0;
    drawCircle ctx (streetsToCoordinates "Genuflect" "7:30") 6.0;
    drawCircle ctx (streetsToCoordinates "Inspirit" "6:00") 6.0;
    drawCircle ctx (streetsToCoordinates "Genuflect" "4:30") 6.0;
    drawCircle ctx (streetsToCoordinates "Genuflect" "3:00") 6.0;
    /* Cursor */
    Canvas.Ctx.setFillStyle ctx "#f00";
    Canvas.Ctx.fillRect ctx (!cursorX -. 2.5) (!cursorY -. 2.5) 5.0 5.0;
    /* Path */
    drawPath !path;
    /* Label */
    Canvas.Ctx.setFillStyle ctx "#000";
    Canvas.Ctx.fillText
      ctx
      (!cursorRadialStreet ^ " & " ^ (!cursorArcStreet == " " ? "Esplanade" : !cursorArcStreet))
      !cursorX
      (!cursorY -. 16.0);
    /* Loop */
    Canvas.requestAnimationFrame render
  };
  let updatePath source dest =>
    path := (
      if (Hashtbl.mem dijkstraMemo (source, dest)) {
        Hashtbl.find dijkstraMemo (source, dest)
      } else {
        let result = Dijkstra.dijkstra max_int 0 (+) graph source dest;
        Hashtbl.replace dijkstraMemo (source, dest) result;
        result
      }
    );
  Canvas.addEventListener
    canvas
    "mousemove"
    (
      fun event => {
        let (arcStreetIndex, radialStreetIndex) = eventToStreetIndices event;
        let (a, b) =
          streetsToCoordinates arcStreets.(arcStreetIndex) radialStreets.(radialStreetIndex);
        let update =
          arcStreets.(arcStreetIndex) != !cursorArcStreet ||
          radialStreets.(radialStreetIndex) != !cursorRadialStreet;
        cursorArcStreet := arcStreets.(arcStreetIndex);
        cursorRadialStreet := radialStreets.(radialStreetIndex);
        cursorX := a;
        cursorY := b;
        if update {
          let source = (!sourceArcStreet, !sourceRadialStreet);
          let dest = (!cursorArcStreet, !cursorRadialStreet);
          updatePath source dest
        }
      }
    );
  Canvas.addEventListener
    canvas
    "click"
    (
      fun event => {
        let (arcStreetIndex, radialStreetIndex) = eventToStreetIndices event;
        sourceArcStreet := arcStreets.(arcStreetIndex);
        sourceRadialStreet := radialStreets.(radialStreetIndex);
        let source = (!sourceArcStreet, !sourceRadialStreet);
        let dest = (!cursorArcStreet, !cursorRadialStreet);
        Hashtbl.clear dijkstraMemo;
        updatePath source dest
      }
    );
  Canvas.requestAnimationFrame render
};

initialize ();
