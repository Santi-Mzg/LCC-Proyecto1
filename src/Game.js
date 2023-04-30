import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult } from './util';
import Square from './Square';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);
  const [drawingPath, setDrawingPath] = useState(false);
  const [nextSquareValue, setNextSquareValue] = useState(0);

  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    if (newPath.length === 0) { // Caso en donde se cancela el recorrido Path.
      setDrawingPath(false);
    }
    else {
      setDrawingPath(true);
    }
    setPath(newPath);
    setNextSquareValue(joinResult(newPath, grid, numOfColumns)); // Actualiza el valor que se va "generando" al ir conectando bloques progresivamente.
    console.log(JSON.stringify(newPath));
  }

  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    /*
    Build Prolog query, which will be like:
    join([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ], 
          5, 
          [[2, 0], [3, 0], [4, 1], [3, 1], [2, 1], [1, 1], [1, 2], [0, 3]],
          RGrids
        ).
    */
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setDrawingPath(false);
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setNextSquareValue(0);
        setPath([]);
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 1000);
    } else {
      setWaiting(false);
    }
  }

  if (grid === null) {
    return null;
  }
  if (drawingPath) {
    return (
      <div className="game">
        <div className="header"> 
          <Square
              value={nextSquareValue}
              className={"result"}
          />
        </div>
        <Board
          grid={grid}
          numOfColumns={numOfColumns}
          path={path}
          onPathChange={onPathChange}
          onDone={onPathDone}
        />
      </div>
    );
  }
  else {
    return (
      <div className="game">
        <div className="header">
          <div className="score">{score}</div>
        </div>
        <Board
          grid={grid}
          numOfColumns={numOfColumns}
          path={path}
          onPathChange={onPathChange}
          onDone={onPathDone}
        />
      </div>
    );
  }
}

export default Game;