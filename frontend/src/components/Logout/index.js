import React from "react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faSignOutAlt } from "@fortawesome/free-solid-svg-icons";

import { ActionButton } from "../../styles/Buttons";

export function Logout({ callback }) {
  return (
    <ActionButton onClick={callback}>
      <FontAwesomeIcon icon={faSignOutAlt} />
    </ActionButton>
  );
}

export default Logout;
