import React from "react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faPlusCircle } from "@fortawesome/free-solid-svg-icons";
import { Link } from "react-router-dom";

import styled from "styled-components";

const Circle = styled.div`
  > a {
    font-size: 2em;
  }
`;

export function Add() {
  return (
    <Circle>
      <Link to="/create">
        <FontAwesomeIcon icon={faPlusCircle} />
      </Link>
    </Circle>
  );
}

export default Add;
