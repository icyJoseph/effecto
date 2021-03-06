import React from "react";
import styled from "styled-components";
import PublicWs from "../PublicWs";

const CreateContent = styled.div`
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

export function Create() {
  return (
    <CreateContent>
      <div className="centered">Create a meeting here</div>
      <PublicWs />
    </CreateContent>
  );
}

export default Create;
