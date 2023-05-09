import React from 'react';
import Square from './Square';

function HeaderDisplay ({ drawingPath, score, nextSquareValue }) {
    if(drawingPath) {
        return (
            <Square
                className="nextSquare"
                value={nextSquareValue}
            />
        )
    }
    else {
        return (
            <div className="score">{score}</div>
        )
    }
}

export default HeaderDisplay;