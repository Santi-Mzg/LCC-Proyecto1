import React from 'react';

function MovidaMaximaButton({ onClick, disabled }) {
    return (
        <button type="button" className="movidaMaximaButton" onClick={onClick} disabled={disabled}>
            Ayuda Movida Máxima
        </button>
    )
}

export default MovidaMaximaButton;