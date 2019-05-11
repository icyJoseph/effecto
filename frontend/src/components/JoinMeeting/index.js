import React from "react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faSignInAlt } from "@fortawesome/free-solid-svg-icons";
import { Link } from "react-router-dom";

import styled from "styled-components";

const Circle = styled.div`
  > a {
    font-size: 2em;
  }
`;

export function JoinMeeting() {
  return (
    <Circle>
      <Link to="/join">
        <FontAwesomeIcon icon={faSignInAlt} />
      </Link>
    </Circle>
  );
}

export default JoinMeeting;
