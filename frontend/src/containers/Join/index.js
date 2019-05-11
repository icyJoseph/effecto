import React from "react";
import styled from "styled-components";
import PublicWs from "../PublicWs";

const JoinContent = styled.div`
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  flex: 1;
  width: 100%;
  height: 100vh;

  > .centered {
    width: 100%;
    text-align: center;
  }
`;

export function Join() {
  return (
    <JoinContent>
      <div className="centered">Join a meeting here</div>
      <PublicWs />
    </JoinContent>
  );
}

export default Join;
