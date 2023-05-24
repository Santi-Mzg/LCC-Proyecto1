import React from 'react';

function MaximosIgualesAdyacentesButton({ onClick, disabled }) {
    return (
        <button type="button" className="maximosIgualesAdyacentesButton" onClick={onClick} disabled={disabled}>
            Ayuda Máximos Iguales Adyacentes
        </button>
    )
}

export default MaximosIgualesAdyacentesButton;