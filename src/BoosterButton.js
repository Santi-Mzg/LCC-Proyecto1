import React from 'react';

function BoosterButton({ onClick, disabled }) {
    return (
        <button type="button" className="boosterButton" onClick={onClick} disabled={disabled}>
            Booster Colapsar Iguales
        </button>
    )
}

export default BoosterButton;