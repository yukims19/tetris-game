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
    | None => true
    | Some(value) => value > 0 ? true : false
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
    |> List.map(((col, row)) => (col + piece.dCol, row + piece.dRow));

  let getAbsoluteRows = piece: list(int) =>
    getShapePoints(piece)
    |> List.map(((_col, row)) => row + piece.dRow)
    |> List.sort_uniq((first, later) => compare(first, later));

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
        switch (isFill(gameBoard, row, col)) {
        | false => checkPoint(tail)
        | true => true
        };
      };
    checkPoint(absPoints);
  };

  let isRowFull = (gameBoard: t, row): bool => {
    let emptyBlockNum =
      Array.to_list(gameBoard[row])
      |> List.filter(block => block == 0)
      |> List.length;
    emptyBlockNum == 0;
  };

  let newEmptyRow = (gameBoard: t) => {
    let colNum = colNum(gameBoard);
    let newRow = Array.make(colNum, 0);
    newRow[0] = wallValue;
    newRow[colNum - 1] = wallValue;
    newRow;
  };

  let emptyRow = (gameBoard: t, row) => {
    print_endline("emptyRow");
    for (r in row downto 0) {
      print_endline("loop");
      print_endline(string_of_int(r));
      switch (r) {
      | 0 =>
        print_endline("newEmptyRow");
        gameBoard[r] = newEmptyRow(gameBoard);
      | _ =>
        print_endline("Replace!!");
        gameBoard[r] = newEmptyRow(gameBoard);
        gameBoard[r] = gameBoard[r - 1];
      };
    };
    ();
  };

  let removeIfPieceFillRow = (gameBoard: t, piece) => {
    getAbsoluteRows(piece)
    |> List.iter(row =>
         switch (isRowFull(gameBoard, row)) {
         | false => ()
         | true => emptyRow(gameBoard, row)
         }
       );
    gameBoard;
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
