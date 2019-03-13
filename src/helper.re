module Board = {
  type t = array(array(int));
  type blockValue = int;
  type piece = {
    shape: Shapes.shape,
    rotation: int,
    dRow: int,
    dCol: int,
  };
  let wallValue = 9;

  let rowNum = (gameBoard: t) => Array.length(gameBoard);

  let colNum = (gameBoard: t) => Array.length(gameBoard[0]);

  let printPoint = ((col, row)) => Printf.printf("%d, %d\n", col, row);

  let get = (gameBoard: t, row, col): option(blockValue) => {
    let isInBound =
      col >= 0 && col < colNum(gameBoard) && row < rowNum(gameBoard);
    isInBound ? Some(gameBoard[row][col]) : None;
  };

  let set = (gameBoard: t, row, col, value: int) => {
    gameBoard[row][col] = value;
    ();
  };

  let makeWalls = (gameBoard: t, row, col) => {
    gameBoard
    |> Array.iteri((idx, _row) => {
         set(gameBoard, idx, 0, wallValue);
         set(gameBoard, idx, col - 1, wallValue);
       });
    gameBoard[row - 1]
    |> Array.iteri((idx, _block) => set(gameBoard, row - 1, idx, wallValue));
  };

  let init = (row, col): t => {
    let board = Array.init(row, _idx => Array.make(col, 0));
    makeWalls(board, row, col);
    board;
  };

  let isFill = (gameBoard: t, row, col): bool =>
    switch (get(gameBoard, row, col)) {
    | None =>
      print_endline("None");
      true;
    | Some(value) =>
      print_endline("Some");
      value > 0 ? true : false;
    };

  let printBoard = gameBoard =>
    Array.iter(
      row => {
        Array.iter(col => print_int(col), row);
        print_newline();
      },
      gameBoard,
    );

  let getShapePoints = piece: list(Shapes.point) => {
    let shapePoints =
      List.nth(piece.shape |> Shapes.getTargetShape, piece.rotation);
    shapePoints;
  };

  let getAbsolutePoints = piece: list(Shapes.point) =>
    getShapePoints(piece)
    |> List.map(point => {
         let (col, row) = point;
         (col + piece.dCol, row + piece.dRow);
       });

  let setPiece = (gameBoard: t, piece: piece) => {
    getAbsolutePoints(piece)
    |> List.iter(point => {
         let (col, row) = point;
         set(gameBoard, row, col, 1);
         ();
       });
    gameBoard;
  };

  let isCollide = (gameBoard: t, piece): bool => {
    let absPoints = getAbsolutePoints(piece);
    let rec checkPoint = points =>
      switch (points) {
      | [] => false
      | [head, ...tail] =>
        let (col, row) = head;
        printPoint(head);
        switch (isFill(gameBoard, row, col)) {
        | false => checkPoint(tail)
        | true => true
        };
      };
    checkPoint(absPoints);
  };
};

/* TODO:
   [x] represent shapes as points
   [x] piece:{
      points: list(point)
      rotation: int,
      dRow: int,
   [x] fill walls
   [x] setPiece(board, piece)
   [x] printBoard
   [x] isCollide (board, piece)
   }*/
