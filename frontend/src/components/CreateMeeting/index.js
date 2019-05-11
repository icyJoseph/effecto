import React from "react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faPlus } from "@fortawesome/free-solid-svg-icons";
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

export function CreateMeeting() {
  return (
    <Circle>
      <Link to="/create">
        <FontAwesomeIcon icon={faPlus} />
        Create Meeting
      </Link>
    </Circle>
  );
}

export default CreateMeeting;
