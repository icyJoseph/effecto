import React from "react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faSignInAlt } from "@fortawesome/free-solid-svg-icons";
import { Link } from "react-router-dom";

import styled from "styled-components";

const Circle = styled.div`
  > a {
    display: flex;
    align-items: center;
    flex-direction: column;
    font-size: 2em;
  }
`;

export function JoinMeeting() {
  return (
    <Circle>
      <Link to="/join">
        <FontAwesomeIcon icon={faSignInAlt} />
        Join Meeting
      </Link>
    </Circle>
  );
}

export default JoinMeeting;
