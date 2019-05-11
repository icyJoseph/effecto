import React, { useEffect, useCallback } from "react";
import { useDispatch, useSelector } from "react-redux";
import PrivateWs from "../PrivateWs";
import LinkedIn from "../../components/LinkedIn";
import Logout from "../../components/Logout";
import Avatar from "../../components/Avatar";
import { GET_PROFILE, LOG_OUT } from "../../ducks/auth";
import { Top } from "./styled";
import CreateMeeting from "../../components/CreateMeeting";
import JoinMeeting from "../../components/JoinMeeting";
import Spinner from "../../components/Spinner";

import styled from "styled-components";

const Content = styled.div`
  text-align: center;
  width: 100%;

  .options {
    display: flex;
    flex-wrap: wrap;
    justify-content: center;

    > * {
      margin: 0 1em;
    }
  }
`;

export function Main() {
  const { done, refresh, ...auth } = useSelector(({ auth }) => auth);
  const { meetings } = useSelector(({ meetings }) => meetings);

  const dispatch = useDispatch();
  const logout = useCallback(() => dispatch({ type: LOG_OUT }), [dispatch]);

  useEffect(() => {
    const now = new Date();
    const then = new Date(refresh);

    if (now - then < 0) {
      !done && dispatch({ type: GET_PROFILE });
    }
    // otherwise, suggest refreshing the session
  }, [dispatch, done, refresh]);

  const { firstName, profilePicture = {}, loadingProfile } = auth;

  return (
    <div id="main">
      {loadingProfile ? (
        <Spinner noContainer={true} />
      ) : (
        <Top>
          <div>
            <h1>Hello {firstName || "Effecto"}!</h1>
          </div>
          <Avatar show={!!done} src={profilePicture.avatar} alt={firstName} />
          <div>
            <code>Log {done ? "out" : "in with"}</code>
            {done ? <Logout callback={logout} /> : <LinkedIn />}
          </div>
        </Top>
      )}
      {done && (
        <Content>
          <div>
            {meetings.length ? (
              meetings.map(meeting => <span key={meeting}>{meeting}</span>)
            ) : (
              // meetings.map(meeting => <span key={meeting}>{meeting}</span>)
              <span>No meetings yet!</span>
            )}
          </div>
          {/* <ul>
        <li>Something Interesting?</li>
      </ul> */}
          <PrivateWs />
          <div className="options">
            <CreateMeeting />
            <JoinMeeting />
          </div>
        </Content>
      )}
    </div>
  );
}

export default Main;
