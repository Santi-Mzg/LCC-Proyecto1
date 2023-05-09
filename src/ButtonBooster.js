import React from 'react';

function ButtonBooster({ onClick, disabled }) {
    return (
        <button type="button" className="buttonBooster" onClick={onClick} disabled={disabled}>
            Booster Colapsar Iguales
        </button>
    )
}

export default ButtonBooster;