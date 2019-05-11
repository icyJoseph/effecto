import React from "react";
import styled, { withTheme } from "styled-components";
import Loader from "react-loader-spinner";

const SpinnerContainer = styled.div`
  position: absolute;
  min-height: 100vh;
  width: 100%;
  display: flex;
  flex: 1;
  flex-direction: column;
  justify-content: center;
  align-items: center;
`;

const Centered = styled.div`
  > * {
    text-align: center;
  }
`;

export function Spinner({ theme: { spinner }, noContainer = false }) {
  return noContainer ? (
    <Centered>
      <Loader type="ThreeDots" color={spinner} height="100" width="100" />
    </Centered>
  ) : (
    <SpinnerContainer>
      <Loader type="ThreeDots" color={spinner} height="100" width="100" />
    </SpinnerContainer>
  );
}

export default withTheme(Spinner);
