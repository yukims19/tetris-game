open Reprocessing;
open Helper;

type state = {
  board: Board.t,
  piece: Board.piece,
};

type action =
  | Right
  | Left
  | Down
  | Rotate
  | Reset
  | Tick
  | Other;

let canvasWidth = 300;
let canvasHeight = 400;
let rectWidth = 20;
let rectHeight = 20;
let timeTick = ref(0.0);
let shapes: list(Shapes.shape) = [Z, S, T, O, L, I, J];

let makeInitialState = () => {
  timeTick := 0.0;
  let gameBoard = Board.init(15, 10);
  let tempPiece: Board.piece = {shape: Z, rotation: 0, dRow: 0, dCol: 2};
  {board: gameBoard, piece: tempPiece};
};

let setup = env => {
  Env.size(~width=canvasWidth, ~height=canvasHeight, env);
  makeInitialState();
};

let makeNewPiece = (): Board.piece => {
  let totalShapeNum = List.length(shapes);
  let newShape = List.nth(shapes, Random.int(totalShapeNum));
  {shape: newShape, rotation: 0, dRow: 0, dCol: 2};
};

let drawGameBoard = (gameBoard, env) =>
  Array.iteri(
    (idxR, row) =>
      Array.iteri(
        (idxC, col) =>
          switch (col) {
          | 0 =>
            Draw.noFill(env);
            Draw.stroke(Constants.black, env);
            Draw.rect(
              ~pos=(idxC * rectHeight, idxR * rectWidth),
              ~width=rectWidth,
              ~height=rectHeight,
              env,
            );
          | 9 => ()
          | _ =>
            Draw.fill(Constants.black, env);
            Draw.stroke(Constants.black, env);
            Draw.rect(
              ~pos=(idxC * rectHeight, idxR * rectWidth),
              ~width=rectWidth,
              ~height=rectHeight,
              env,
            );
          },
        row,
      ),
    gameBoard,
  );

let fillSquare = ((col, row), color, env) => {
  Draw.fill(color, env);
  Draw.rect(
    ~pos=(col * rectHeight, row * rectWidth),
    ~width=rectWidth,
    ~height=rectHeight,
    env,
  );
  ();
};

let fillPiece = (piece, env) => {
  let points = Board.getShapePoints(piece);
  List.iter(
    ((col, row)) =>
      fillSquare((piece.dCol + col, piece.dRow + row), Constants.black, env),
    points,
  );
};

let increaseDeltaRow = ({piece} as state): state => {
  ...state,
  piece: {
    shape: piece.shape,
    rotation: piece.rotation,
    dRow: piece.dRow + 1,
    dCol: piece.dCol,
  },
};

let decreaseDeltaRow = ({piece} as state): state => {
  ...state,
  piece: {
    shape: piece.shape,
    rotation: piece.rotation,
    dRow: piece.dRow - 1,
    dCol: piece.dCol,
  },
};

let increaseDeltaCol = ({piece} as state): state => {
  ...state,
  piece: {
    shape: piece.shape,
    rotation: piece.rotation,
    dRow: piece.dRow,
    dCol: piece.dCol + 1,
  },
};

let decreaseDeltaCol = ({piece} as state): state => {
  ...state,
  piece: {
    shape: piece.shape,
    rotation: piece.rotation,
    dRow: piece.dRow,
    dCol: piece.dCol - 1,
  },
};

let rotatePiece = ({piece} as state): state => {
  let variationNum = Shapes.getShapeVariationNum(piece.shape);
  let newRotation = (piece.rotation + 1) mod variationNum;
  {
    ...state,
    piece: {
      shape: piece.shape,
      rotation: newRotation,
      dRow: piece.dRow,
      dCol: piece.dCol,
    },
  };
};

let freezePiece = ({piece} as state, env): state => {
  fillPiece(piece, env);
  let newBoard = Board.setPiece(state.board, state.piece);
  {...state, board: newBoard};
};

let envToAction = env =>
  switch (Env.keyCode(env)) {
  | Space => Reset
  | Right => Right
  | Left => Left
  | Down => Down
  | Up => Rotate
  | _ => Other
  };

let draw = (state, env) => {
  timeTick := timeTick^ +. Env.deltaTime(env);
  Draw.background(Constants.white, env);
  drawGameBoard(state.board, env);
  fillPiece(state.piece, env);
  /*         if (timeTick^ > 1.0) {
                                  /*increaseDeltaRow();*/
                                  timeTick := 0.0;
             };*/
  state;
};

run(
  ~setup,
  ~draw,
  ~keyTyped=
    (state, env) => {
      print_endline("pressed");
      let action =
        switch (envToAction(env)) {
        | Reset => (_state => makeInitialState())
        | Right => (
            state => {
              let nextState = increaseDeltaCol(state);
              switch (Board.isCollide(nextState.board, nextState.piece)) {
              | false => nextState
              | true => state
              };
            }
          )
        | Left => (
            state => {
              let nextState = decreaseDeltaCol(state);
              switch (Board.isCollide(nextState.board, nextState.piece)) {
              | false => nextState
              | true => state
              };
            }
          )
        | Down => (
            state => {
              let nextState = increaseDeltaRow(state);
              switch (Board.isCollide(nextState.board, nextState.piece)) {
              | false => nextState
              | true =>
                let newState = freezePiece(state, env);
                let newPiece = makeNewPiece();
                {...newState, piece: newPiece};
              };
            }
          )
        | Rotate => (
            state => {
              let nextState = rotatePiece(state);
              switch (Board.isCollide(nextState.board, nextState.piece)) {
              | false => nextState
              | true => state
              };
            }
          )
        | Tick
        | Other => (state => state)
        };
      action(state);
    },
  (),
);
